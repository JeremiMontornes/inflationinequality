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
  if (!has_internet()) {
    skip("No internet connection")
  }
}

# Mock functions to simulate data loading
mock_load_cpi <- function(country, level, start_year, start_month, end_year, end_month) {
  cpi_fr2 <- if (!is.null(start_year) &&
                 start_year == 2016 &&
                 !is.null(end_year) &&
                 end_year == 2017) {
    readRDS(test_path("fixtures", "cpi_fr2_s2016_e2017.RDS"))
  } else if (!is.null(start_year) &&
             start_year == 2014 &&
             !is.null(end_year) &&
             end_year == 2017) {
    readRDS(test_path("fixtures", "cpi_fr2_s2014_e2017.RDS"))
  } else {
    readRDS("fixtures/cpi_fr2.RDS")
  }
  return(cpi_fr2)
}

mock_calculate_weights <- function(country, category, level, start_year, end_year,
                                   custom_index_weights,
                                   custom_hbs,
                                   interpolated_hbs,
                                   specific_hbs_year) {
  weights_fr2 <- if (category == "income") {
    if (!is.null(start_year) &&
        start_year == 2016 &&
        !is.null(end_year) &&
        end_year == 2017) {
      readRDS("fixtures/weights_fr_income2_s2016_e2017.RDS")
    } else if (!is.null(start_year) &&
               start_year == 2014 &&
               !is.null(end_year) &&
               end_year == 2017) {
      readRDS("fixtures/weights_fr_income2_s2014_e2017.RDS")
    } else {
      readRDS("fixtures/weights_fr_income2.RDS")
    }
  } else if (category == "age") {
    readRDS("fixtures/weights_fr_age2.RDS")
  } else {
    readRDS("fixtures/weights_fr_urban2.RDS")
  }
  return(weights_fr2)
}

test_that("calculate_contributions input validation works", {
  skip_if_no_internet()
  expect_error(calculate_contributions("FRA", "income"), "Country must be a 2-character ISO code")
  expect_error(calculate_contributions("FR", "invalid"), "Category must be one of 'income', 'age', or 'urban'")
  expect_error(calculate_contributions("FR", "income", level = 4), "Level must be an integer between 1 and 3")
})

test_that("calculate_contributions returns expected structure", {
  local_mocked_bindings(load_cpi = mock_load_cpi, .package = "inflationinequality")
  local_mocked_bindings(calculate_weights = mock_calculate_weights, .package = "inflationinequality")
  result <- calculate_contributions("FR", "income")
  expect_s3_class(result$dt, "data.table")
  expect_named(result$dt, c("coicop", "category", "year", "month", "contribution"))
})

test_that("calculate_contributions handles different date ranges", {
  local_mocked_bindings(load_cpi = mock_load_cpi, .package = "inflationinequality")
  local_mocked_bindings(calculate_weights = mock_calculate_weights, .package = "inflationinequality")
  result_full <- calculate_contributions("FR", "income")
  result_partial <- calculate_contributions("FR", "income", start_year = 2016, end_year = 2017)

  expect_lte(nrow(result_partial$dt), nrow(result_full$dt))
  expect_in(result_partial$dt$year, 2016:2017)  # Remember, it calculates for y-2 years
})

test_that("calculate_contributions works with different categories", {
  local_mocked_bindings(load_cpi = mock_load_cpi, .package = "inflationinequality")
  local_mocked_bindings(calculate_weights = mock_calculate_weights, .package = "inflationinequality")
  result_income <- calculate_contributions("FR", "income")
  result_age <- calculate_contributions("FR", "age")
  result_urban <- calculate_contributions("FR", "urban")

  expect_false(identical(result_income$dt, result_urban$dt))
})

test_that("calculate_contributions with sideloaded CPI data fails with mismatched dates", {
  local_mocked_bindings(load_cpi = mock_load_cpi, .package = "inflationinequality")
  local_mocked_bindings(calculate_weights = mock_calculate_weights, .package = "inflationinequality")
  dt_cpi_fr <- load_cpi("FR",start_year = 2016, end_year = 2017)
  expect_error(calculate_contributions("FR", "income", start_year = 2015, custom_cpi = dt_cpi_fr))
})

test_that("calculate_contributions does not mix up data between categories: single category", {
  skip_if_no_internet()
  hbs <- load_hbs("FR", "income")
  dt_reduced_hbs <- hbs$dt[category == "Fifth quintile"]
  reduced_hbs <- hbs(
    dt = dt_reduced_hbs, dt_total = hbs$dt_total,
    country = hbs$category, category = hbs$category,
    categories = "Fifth quintile", level = 2
  )

  contributions <- calculate_contributions("FR", "income", custom_hbs = hbs)
  reduced_contributions <- calculate_contributions("FR", "income", custom_hbs = reduced_hbs)

  dt_contributions <- contributions$dt[category == "Fifth quintile"]
  dt_reduced_contributions <- reduced_contributions$dt[, .(coicop, year, month, category, reduced_contribution = contribution)]

  # Merge the two data.tables
  merged_dt <-
    dt_contributions[dt_reduced_contributions, on = .(coicop, year, month, category)]

  # Compare contributions
  merged_dt[, match := contribution == reduced_contribution]

  # Check for mismatches
  mismatches <- merged_dt[match == FALSE | is.na(match)]

  # Test assertion
  expect_equal(nrow(mismatches), 0,
               info = "Mismatches found in contributions between the two data.tables")
})

test_that("calculate_contributions does not mix up data between categories: random order of categories", {
  skip_if_no_internet()
  hbs <- load_hbs("FR", "income")
  reduced_hbs <- hbs(
    dt = hbs$dt, dt_total = hbs$dt_total,
    country = hbs$category, category = hbs$category,
    categories = c("Third quintile", "Fifth quintile", "Fourth quintile", "Second quintile", "First quintile"), level = 2
  )

  contributions <- calculate_contributions("FR", "income", custom_hbs = hbs)
  reduced_contributions <- calculate_contributions("FR", "income", custom_hbs = reduced_hbs)

  dt_contributions <- contributions$dt
  dt_reduced_contributions <- reduced_contributions$dt[, .(coicop, year, month, category, reduced_contribution = contribution)]

  # Merge the two data.tables
  merged_dt <-
    dt_contributions[dt_reduced_contributions, on = .(coicop, year, month, category)]

  # Compare contributions
  merged_dt[, match := contribution == reduced_contribution]

  # Check for mismatches
  mismatches <- merged_dt[match == FALSE | is.na(match)]

  # Test assertion
  expect_equal(nrow(mismatches), 0,
               info = "Mismatches found in contributions between the two data.tables")
})
