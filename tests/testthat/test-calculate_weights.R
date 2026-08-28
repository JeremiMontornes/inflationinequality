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
  categories <- c(
    "First quintile", "Second quintile", "Third quintile",
    "Fourth quintile", "Fifth quintile"
  )
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
    country = "FR",
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
    country = "FR",
    level = 1,
    base_total = 1000
  )

  result <- calculate_weights(
    "FR", "income",
    level = 1,
    custom_index_weights = custom_index_weights,
    custom_hbs = custom_hbs,
    weighting_method = "ras"
  )

  row_sums <- result$dt[, .(total = sum(weighted_consumption)), by = category]
  expect_true(all(abs(row_sums$total - 100) < 1e-8))

  expect_true(all(c("weighted_mass", "category_share") %in% names(result$dt)))
  expect_equal(
    result$dt$weighted_mass,
    result$dt$category_share * result$dt$weighted_consumption,
    tolerance = 1e-10
  )
  expect_equal(
    result$dt[, sum(weighted_mass), by = coicop][order(coicop)]$V1,
    c(60, 40),
    tolerance = 1e-8
  )

  category_shares <- inflationinequality:::ras_category_shares(
    "income",
    country = "FR",
    categories = categories,
    weight_years = 2022L
  )
  actual_shares <- unique(
    result$dt[, .(category, weight_year, category_share)]
  )
  data.table::setorder(actual_shares, category, weight_year)
  data.table::setorder(category_shares, category, weight_year)
  expect_equal(actual_shares, category_shares, tolerance = 1e-10)

  aggregate_weights <- result$dt[
    ,
    .(weighted_average = sum(weighted_consumption * category_share)),
    by = coicop
  ][order(coicop)]
  expect_equal(aggregate_weights$weighted_average, c(60, 40), tolerance = 1e-8)
})

test_that("calculate_weights additive QP preserves both margins and non-negativity", {
  categories <- c(
    "First quintile", "Second quintile", "Third quintile",
    "Fourth quintile", "Fifth quintile"
  )
  hbs_dt <- data.table::CJ(
    coicop = c("01", "02", "03"),
    year = 2020,
    category = categories
  )
  hbs_dt[, series_name := "test HBS"]
  # The first group puts nearly all expenditure on item 01. Combined with its
  # small HICP margin, the unconstrained additive solution contains negatives.
  hbs_dt[, consumption := data.table::fcase(
    category == categories[1L] & coicop == "01", 850,
    category == categories[1L], 75,
    coicop == "01", 0,
    default = 500
  )]
  data.table::setcolorder(
    hbs_dt,
    c("series_name", "coicop", "year", "category", "consumption")
  )
  custom_hbs <- hbs(
    hbs_dt,
    data.table::data.table(
      series_name = "test HBS total",
      coicop = c("01", "02", "03"),
      year = 2020,
      total_consumption = c(170, 415, 415)
    ),
    country = "FR",
    category = "income",
    categories = categories,
    level = 1
  )
  custom_index_weights <- index_weights(
    data.table::data.table(
      coicop = c("01", "02", "03"),
      weight = c(100, 450, 450),
      year = 2022
    ),
    country = "FR",
    level = 1,
    base_total = 1000
  )

  result <- calculate_weights(
    "FR", "income",
    level = 1,
    custom_index_weights = custom_index_weights,
    custom_hbs = custom_hbs,
    weighting_method = "additive_qp"
  )
  shares <- inflationinequality:::ras_category_shares(
    "income", "FR", categories, 2022L
  )
  checked <- merge(result$dt, shares, by = c("category", "weight_year"))

  expect_gte(min(checked$weighted_consumption), -1e-10)
  expect_equal(
    checked[, sum(weighted_consumption), by = category]$V1,
    rep(100, length(categories)),
    tolerance = 1e-8
  )
  expect_equal(
    checked[, sum(category_share * weighted_consumption), by = coicop][order(coicop)]$V1,
    c(10, 45, 45),
    tolerance = 1e-8
  )
})

test_that("additive QP leaves an already feasible additive solution unchanged", {
  seed <- matrix(
    c(55, 45, 60, 40),
    nrow = 2,
    byrow = TRUE,
    dimnames = list(c("low", "high"), c("01", "02"))
  )
  result <- inflationinequality:::additive_qp_project(
    seed,
    category_share = c(0.5, 0.5),
    hicp_target = c(57.5, 42.5)
  )
  expect_equal(result, seed, tolerance = 1e-10)
})

test_that("additive QP treats an item without HBS support neutrally", {
  categories <- c("low", "high")
  dt <- data.table::CJ(category = categories, coicop = c("01", "02"))
  dt[, `:=`(
    weight_year = 2022L,
    weight = data.table::fifelse(coicop == "01", 700, 300),
    category_share = 0.5,
    consumption = data.table::fcase(
      coicop == "02", 1e-6,
      category == "low", 80,
      default = 120
    ),
    total_consumption = data.table::fifelse(coicop == "01", 100, 1e-6)
  )]
  result <- inflationinequality:::additive_qp_calibrate_group(
    dt, categories, tolerance = 1e-10, max_iter = 10000L
  )
  expect_equal(
    result[coicop == "02", weighted_consumption],
    c(30, 30),
    tolerance = 1e-8
  )
})

test_that("additive QP combines actual and imputed rents without double counting", {
  categories <- c("low", "high")
  dt <- data.table::data.table(
    series_name = "national HBS",
    coicop = rep(c("041", "042", "045"), each = 2L),
    year = 2022,
    category = rep(categories, 3L),
    consumption = c(20, 5, 10, 30, 15, 25)
  )
  dt_total <- data.table::data.table(
    series_name = "national HBS total",
    coicop = c("041", "042", "045"),
    year = 2022,
    total_consumption = c(12.5, 20, 20)
  )
  original <- hbs(
    dt, dt_total,
    country = "ZZ", category = "income", categories = categories, level = 2
  )

  combined <- inflationinequality:::combine_hbs_actual_and_imputed_rents(original)

  expect_false("042" %in% combined$dt$coicop)
  expect_false("042" %in% combined$dt_total$coicop)
  expect_equal(
    combined$dt[coicop == "041"][order(category), consumption],
    c(35, 30)
  )
  expect_equal(combined$dt_total[coicop == "041", total_consumption], 32.5)
  expect_equal(sum(combined$dt$consumption), sum(original$dt$consumption))
  expect_equal(
    sum(combined$dt_total$total_consumption),
    sum(original$dt_total$total_consumption)
  )
  expect_true(combined$combined_hbs_housing_041_042)
})

test_that("housing combination is a no-op when imputed rents are unavailable", {
  categories <- c("low", "high")
  original <- hbs(
    data.table::data.table(
      series_name = "HBS", coicop = "041", year = 2022,
      category = categories, consumption = c(10, 20)
    ),
    data.table::data.table(
      series_name = "HBS total", coicop = "041", year = 2022,
      total_consumption = 15
    ),
    country = "ZZ", category = "income", categories = categories, level = 2
  )
  expect_identical(
    inflationinequality:::combine_hbs_actual_and_imputed_rents(original),
    original
  )
})

test_that("level-2 housing bridge is applied by every weighting method", {
  categories <- c(
    "First quintile", "Second quintile", "Third quintile",
    "Fourth quintile", "Fifth quintile"
  )
  hbs_dt <- data.table::CJ(
    coicop = c("041", "042", "045"),
    year = 2020,
    category = categories
  )
  hbs_dt[, `:=`(
    series_name = "test HBS",
    consumption = data.table::fcase(
      coicop == "041", c(30, 25, 20, 15, 10)[match(category, categories)],
      coicop == "042", c(5, 10, 15, 20, 25)[match(category, categories)],
      default = 40
    )
  )]
  custom_hbs <- hbs(
    hbs_dt,
    data.table::data.table(
      series_name = "test HBS total",
      coicop = c("041", "042", "045"),
      year = 2020,
      total_consumption = c(20, 15, 40)
    ),
    country = "FR", category = "income", categories = categories, level = 2
  )
  custom_index_weights <- index_weights(
    data.table::data.table(
      coicop = c("041", "045"), weight = c(400, 600), year = 2022
    ),
    country = "FR", level = 2, base_total = 1000
  )

  for (method in c("relative_expenditure", "ras", "additive_qp")) {
    result <- calculate_weights(
      "FR", "income", level = 2,
      custom_index_weights = custom_index_weights,
      custom_hbs = custom_hbs,
      weighting_method = method
    )
    expect_false("042" %in% result$dt$coicop, info = method)
    expect_equal(
      result$dt[, sum(weighted_consumption), by = category]$V1,
      rep(100, length(categories)),
      tolerance = 1e-8,
      info = method
    )
    expect_equal(
      unique(result$dt_coicop_bridge[hicp_coicop == "041", mapping_status]),
      "combined_hbs_041_042",
      info = method
    )
  }
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

  expect_warning(
    result <- calculate_weights(
      "ZZ", "income",
      level = 1,
      custom_index_weights = custom_index_weights,
      custom_hbs = custom_hbs,
      weighting_method = "ras"
    ),
    "using equal group shares"
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

test_that("calculate_weights RAS uses France INSEE level-3 group shares", {
  skip_if_no_internet()

  expected <- list(
    income = readRDS(system.file(
      "extdata", "INSEE_HBS_2017_level3.RDS",
      package = "inflationinequality", mustWork = TRUE
    )),
    age = readRDS(system.file(
      "extdata", "INSEE_HBS_2017_age_level3.RDS",
      package = "inflationinequality", mustWork = TRUE
    )),
    urban = readRDS(system.file(
      "extdata", "INSEE_HBS_2017_urban_level3.RDS",
      package = "inflationinequality", mustWork = TRUE
    ))
  )

  for (hbs_category in names(expected)) {
    hbs_obj <- expected[[hbs_category]]
    expected_shares <- hbs_obj$dt[
      nchar(coicop) == 2L,
      .(category_share = sum(consumption, na.rm = TRUE)),
      by = category
    ]
    expected_shares[, category_share := category_share / sum(category_share)]

    shares <- inflationinequality:::ras_category_shares(
      hbs_category,
      country = "FR",
      categories = hbs_obj$categories,
      weight_years = 2017L
    )

    merged <- merge(expected_shares, shares, by = "category")
    expect_equal(nrow(merged), length(hbs_obj$categories))
    expect_equal(merged$category_share.x, merged$category_share.y, tolerance = 1e-10)

    weights <- calculate_weights(
      "FR", hbs_category,
      level = 3,
      weighting_method = "ras"
    )
    weight_sums <- weights$dt[
      ,
      .(total_weight = sum(weighted_consumption)),
      by = .(category, weight_year)
    ]
    expect_true(all(abs(weight_sums$total_weight - 100) < 1e-6))
  }
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
