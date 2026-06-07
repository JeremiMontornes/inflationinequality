#' Helper function to check internet connectivity
has_internet <- function() {
  tryCatch({
    readLines("https://cran.r-project.org", n = 1)
    TRUE
  }, warning = function(w) FALSE,
  error = function(e) FALSE)
}


#' Custom skip function
skip_if_no_internet <- function() {
  testthat::skip_on_cran()
  if (identical(Sys.getenv("INFLATIONINEQUALITY_SKIP_NETWORK_TESTS"), "true")) {
    skip("Network tests disabled")
  }
  if (!has_internet()) {
    skip("No internet connection")
  }
}

# Mock functions to simulate data loading
mock_load_index_weights <- function(country, level, start_year, end_year) {
  index_weights_fr <- if (!is.null(start_year) &&
                          start_year == 2016 &&
                          !is.null(end_year) &&
                          end_year == 2017) {
    readRDS("fixtures/index_weights_fr2_s2016_e2017.RDS")
    } else {
      readRDS("fixtures/index_weights_fr2.RDS")
    }
  return(index_weights_fr)
}

mock_load_hbs <- function(country, category, level, start_year, end_year) {
  switch(category,
         "income" = readRDS("fixtures/hbs_fr_income2.RDS"),
         "age" = readRDS("fixtures/hbs_fr_age2.RDS"),
         "urban" = readRDS("fixtures/hbs_fr_urban2.RDS"))
}

test_that("calculate_weights input validation works", {
  skip_if_no_internet()
  expect_error(calculate_weights("FRA", "income"), "Country must be a 2-character ISO code")
  expect_error(calculate_weights("FR", "invalid"), "Category must be one of 'income', 'age', or 'urban'")
  expect_error(calculate_weights("FR", "income", level = 4), "Level must be an integer between 1 and 3")
  expect_error(calculate_weights("FR", "income", level = "2"), "Level must be an integer between 1 and 3")
})

test_that("calculate_weights returns expected structure", {
  local_mocked_bindings(load_index_weights = mock_load_index_weights, .package = "inflationinequality")
  local_mocked_bindings(load_hbs = mock_load_hbs, .package = "inflationinequality")
  result <- calculate_weights("FR", "income")
  expect_s3_class(result, "weights")
  expect_s3_class(result$dt, "data.table")
  expect_named(result$dt, c("series_name", "coicop", "year", "category", "weighted_consumption", "weight_year"), ignore.order = TRUE)
})

test_that("calculate_weights handles missing COICOP codes", {
  local_mocked_bindings(load_index_weights = mock_load_index_weights, .package = "inflationinequality")
  local_mocked_bindings(load_hbs = mock_load_hbs, .package = "inflationinequality")

  # Modify mock function to simulate missing COICOP code
  local_mocked_bindings(load_hbs = function(...) {
    hbs <- mock_load_hbs(...)
    hbs$dt <- hbs$dt[-1, ]  # Remove first row to create a missing COICOP
    hbs
  })

  result <- calculate_weights("FR", "income")
  expect_true(all(c("011", "012", "021") %in% result$dt$coicop))
})

test_that("calculate_weights normalizes weights correctly", {
  local_mocked_bindings(load_index_weights = mock_load_index_weights, .package = "inflationinequality")
  local_mocked_bindings(load_hbs = mock_load_hbs, .package = "inflationinequality")
  result <- calculate_weights("FR", "income")

  # Check if weights sum to 100 for each category and weight_year
  weight_sums <- result$dt[, .(total_weight = sum(weighted_consumption)), by = .(category, weight_year)]
  expect_true(all(abs(weight_sums$total_weight - 100) < 1e-6))
})

test_that("calculate_weights RAS calibrates quintile average to HICP weights", {
  categories <- paste0("Q", 1:5)
  hbs_dt <- data.table::CJ(
    coicop = c("01", "02"),
    year = 2020,
    category = categories
  )
  hbs_dt[, series_name := "test HBS"]
  hbs_dt[, consumption := c(120, 80, 100, 110, 90, 70, 130, 100, 90, 110)]
  data.table::setcolorder(hbs_dt, c("series_name", "coicop", "year", "category", "consumption"))
  hbs_total <- data.table::data.table(
    series_name = "test HBS total",
    coicop = c("01", "02"),
    year = 2020,
    total_consumption = c(100, 100)
  )
  custom_hbs <- hbs(
    hbs_dt,
    hbs_total,
    country = "ZZ",
    category = "income",
    categories = categories,
    level = 1
  )
  custom_index_weights <- index_weights(
    data.table::data.table(
      coicop = c("01", "02"),
      weight = c(600, 400),
      year = 2022
    ),
    country = "ZZ",
    level = 1,
    base_total = 1000
  )

  result <- calculate_weights(
    "ZZ", "income",
    level = 1,
    custom_index_weights = custom_index_weights,
    custom_hbs = custom_hbs,
    weighting_method = "ras"
  )

  row_sums <- result$dt[, .(total = sum(weighted_consumption)), by = category]
  expect_true(all(abs(row_sums$total - 100) < 1e-8))

  aggregate_weights <- result$dt[
    ,
    .(weighted_average = mean(weighted_consumption)),
    by = coicop
  ][order(coicop)]
  expect_equal(aggregate_weights$weighted_average, c(60, 40), tolerance = 1e-8)
})

test_that("calculate_weights RAS works with income deciles", {
  categories <- paste0("D", 1:10)
  hbs_dt <- data.table::CJ(
    coicop = c("01", "02"),
    year = 2020,
    category = categories
  )
  hbs_dt[, series_name := "test HBS"]
  hbs_dt[, consumption := rep(c(80, 120), each = 10)]
  data.table::setcolorder(hbs_dt, c("series_name", "coicop", "year", "category", "consumption"))
  hbs_total <- data.table::data.table(
    series_name = "test HBS total",
    coicop = c("01", "02"),
    year = 2020,
    total_consumption = c(100, 100)
  )
  custom_hbs <- hbs(
    hbs_dt,
    hbs_total,
    country = "ZZ",
    category = "income",
    categories = categories,
    level = 1
  )
  custom_index_weights <- index_weights(
    data.table::data.table(
      coicop = c("01", "02"),
      weight = c(250, 750),
      year = 2022
    ),
    country = "ZZ",
    level = 1,
    base_total = 1000
  )

  result <- calculate_weights(
    "ZZ", "income",
    level = 1,
    custom_index_weights = custom_index_weights,
    custom_hbs = custom_hbs,
    weighting_method = "ras"
  )

  row_sums <- result$dt[, .(total = sum(weighted_consumption)), by = category]
  expect_true(all(abs(row_sums$total - 100) < 1e-8))

  aggregate_weights <- result$dt[
    ,
    .(weighted_average = mean(weighted_consumption)),
    by = coicop
  ][order(coicop)]
  expect_equal(aggregate_weights$weighted_average, c(25, 75), tolerance = 1e-8)
})

test_that("calculate_weights uses bundled Italy reconstructed income HBS when available", {
  local_mocked_bindings(load_index_weights = mock_load_index_weights, .package = "inflationinequality")
  local_mocked_bindings(load_hbs = mock_load_hbs, .package = "inflationinequality")

  result <- calculate_weights("IT", "income")

  expect_equal(sort(unique(result$dt$year)), c(2015, 2020))
  expect_equal(result$categories, c(
    "First quintile", "Second quintile", "Third quintile",
    "Fourth quintile", "Fifth quintile"
  ))
})

# test_that("calculate_weights handles zero values correctly", {
#   local_mocked_bindings(load_index_weights = function(...) {
#     index_weights <- mock_load_index_weights(...)
#     index_weights$dt[1, weight := 0]
#     index_weights
#   })
#
#   local_mocked_bindings(load_hbs = function(...) {
#     hbs <- mock_load_hbs(...)
#     hbs$dt[1, consumption := 0]
#     hbs
#   })
#
#   result <- calculate_weights("FR", "income")
#   expect_true(all(result$dt$weighted_consumption > 0))
# })

# I don't know how to properly test this property.
# test_that("calculate_weights matches correct HBS year", {
#   result <- calculate_weights("FR", "income")
#
#   # Check if each weight_year is matched with the correct HBS year
#   correct_matches <- result$dt[, all(year <= weight_year), by = .(coicop, category, weight_year)]
#   expect_true(all(correct_matches$V1))
#
#   # Check if the most recent HBS year is used for each weight_year
#   most_recent_matches <- result$dt[, .SD[which.max(year)], by = .(coicop, category, weight_year)]
#   expect_equal(nrow(most_recent_matches), nrow(result$dt))
# })

test_that("calculate_weights handles different date ranges", {
  local_mocked_bindings(load_index_weights = mock_load_index_weights, .package = "inflationinequality")
  local_mocked_bindings(load_hbs = mock_load_hbs, .package = "inflationinequality")
  result_full <- calculate_weights("FR", "income")
  result_partial <- calculate_weights("FR", "income", start_year = 2016, end_year = 2017)

  expect_lte(nrow(result_partial$dt), nrow(result_full$dt))
  expect_in(result_partial$dt$weight_year, 2016:2017)
})

test_that("calculate_weights works with different categories", {
  local_mocked_bindings(load_index_weights = mock_load_index_weights, .package = "inflationinequality")
  local_mocked_bindings(load_hbs = mock_load_hbs, .package = "inflationinequality")
  result_income <- calculate_weights("FR", "income")
  result_age <- calculate_weights("FR", "age")
  result_urban <- calculate_weights("FR", "urban")

  expect_false(identical(result_income$dt, result_age$dt))
  expect_false(identical(result_income$dt, result_urban$dt))
  expect_false(identical(result_age$dt, result_urban$dt))
})
