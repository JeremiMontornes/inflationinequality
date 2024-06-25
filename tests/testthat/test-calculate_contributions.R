# Mock functions to simulate data loading
mock_load_cpi <- function(country, level, start_year, start_month, end_year, end_month) {
  data.table::data.table(
    coicop = rep(c("01", "02", "03"), each = 36),
    year = rep(2015:2017, each = 12, times = 3),
    month = rep(1:12, times = 9),
    value = runif(108, 90, 110)
  )
}

mock_calculate_weights <- function(country, category, level, start_year, end_year) {
  data.table::data.table(
    coicop = rep(c("01", "02", "03"), each = 6),
    weight_year = rep(2015:2017, each = 2, times = 3),
    category = rep(c("QUINTILE1", "QUINTILE2"), times = 9),
    weighted_consumption = runif(18, 0, 100)
  )
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
  expect_s3_class(result, "data.table")
  expect_named(result, c("year", "coicop", "month", "category", "contribution"))
})

test_that("calculate_contributions handles different date ranges", {
  result_full <- calculate_contributions("FR", "income")
  result_partial <- calculate_contributions("FR", "income", start_year = 2016, end_year = 2017)

  expect_lte(nrow(result_partial), nrow(result_full))
  expect_in(result_partial$year, 2017)  # Remember, it calculates for y-2 years
})

test_that("calculate_contributions works with different categories", {
  result_income <- calculate_contributions("FR", "income")
  result_age <- calculate_contributions("FR", "age")
  result_urban <- calculate_contributions("FR", "urban")

  expect_false(identical(result_income, result_urban))
})

test_that("calculate_contributions handles missing data correctly", {
  local_mocked_bindings(load_cpi = function(...) {
    dt <- mock_load_cpi(...)
    dt <- dt[!(year == 2016 & coicop == "02")]  # Remove some data
    dt
  })

  expect_warning(calculate_contributions("FR", "income"), "missing data")
})

test_that("has_all_coicop function works correctly", {
  test_data <- data.table::data.table(year = c(2015, 2015, 2015, 2016, 2016),
                          coicop = c("01", "02", "03", "01", "02"))
  expect_true(has_all_coicop(test_data, 2015, c("01", "02", "03")))
  expect_false(has_all_coicop(test_data, 2016, c("01", "02", "03")))
})

test_that("calculate_contributions handles single year of data", {
  local_mocked_bindings(load_cpi = function(...) {
    mock_load_cpi(...)[year == 2017, ]
  })

  local_mocked_bindings(calculate_weights = function(...) {
    mock_calculate_weights(...)[weight_year == 2017, ]
  })

  expect_error(calculate_contributions("FR", "income"), "Insufficient data")
})
