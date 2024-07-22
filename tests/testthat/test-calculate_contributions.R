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


# Replace actual functions with mocks
assignInNamespace("load_cpi", mock_load_cpi, "inflationinequality")
assignInNamespace("calculate_weights", mock_calculate_weights, "inflationinequality")

test_that("calculate_contributions input validation works", {
  expect_error(calculate_contributions("FRA", "income"), "Country must be a 2-character ISO code")
  expect_error(calculate_contributions("FR", "invalid"), "Category must be one of 'income', 'age', or 'urban'")
  expect_error(calculate_contributions("FR", "income", level = 4), "Level must be an integer between 1 and 3")
})

test_that("calculate_contributions returns expected structure", {
  result <- calculate_contributions("FR", "income")
  expect_s3_class(result$dt, "data.table")
  expect_named(result$dt, c("coicop", "category", "year", "month", "contribution"))
})

test_that("calculate_contributions handles different date ranges", {
  result_full <- calculate_contributions("FR", "income")
  result_partial <- calculate_contributions("FR", "income", start_year = 2016, end_year = 2017)

  expect_lte(nrow(result_partial$dt), nrow(result_full$dt))
  expect_in(result_partial$dt$year, 2016:2017)  # Remember, it calculates for y-2 years
})

test_that("calculate_contributions works with different categories", {
  result_income <- calculate_contributions("FR", "income")
  result_age <- calculate_contributions("FR", "age")
  result_urban <- calculate_contributions("FR", "urban")

  expect_false(identical(result_income$dt, result_urban$dt))
})
