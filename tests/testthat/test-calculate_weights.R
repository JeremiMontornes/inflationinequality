# Mock functions to simulate data loading
mock_load_index_weights <- function(country, level, start_year, end_year) {
  data.table::data.table(
    coicop = c("01", "02", "03"),
    year = rep(2015:2017, each = 3),
    weight = runif(9, 0, 100)
  )
}

mock_load_hbs <- function(country, category, level, start_year, end_year) {
  data.table::data.table(
    coicop = c("01", "02", "03"),
    year = rep(c(2015, 2017), each = 3),
    category = rep(c("QUINTILE1", "QUINTILE2"), each = 3),
    consumption = runif(6, 0, 1000),
    series_name = paste0("Series_", 1:6)
  )
}

# Replace actual functions with mocks
assignInNamespace("load_index_weights", mock_load_index_weights, "inflationinequality")
assignInNamespace("load_hbs", mock_load_hbs, "inflationinequality")

test_that("calculate_weights input validation works", {
  expect_error(calculate_weights("FRA", "income"), "Country must be a 2-character ISO code")
  expect_error(calculate_weights("FR", "invalid"), "Category must be one of 'income', 'age', or 'urban'")
  expect_error(calculate_weights("FR", "income", level = 4), "Level must be an integer between 1 and 3")
  expect_error(calculate_weights("FR", "income", level = "2"), "Level must be an integer between 1 and 3")
})

test_that("calculate_weights returns expected structure", {
  result <- calculate_weights("FR", "income")
  expect_s3_class(result, "data.table")
  expect_named(result, c("series_name", "coicop", "year", "category", "weighted_consumption", "weight_year"), ignore.order = TRUE)
})

test_that("calculate_weights handles missing COICOP codes", {
  # Modify mock function to simulate missing COICOP code
  local_mocked_bindings(load_hbs = function(...) {
    mock_load_hbs(...)[-1, ]  # Remove first row to create a missing COICOP
  })

  result <- calculate_weights("FR", "income")
  expect_true(all(c("01", "02", "03") %in% result$coicop))
})

test_that("calculate_weights normalizes weights correctly", {
  result <- calculate_weights("FR", "income")

  # Check if weights sum to 100 for each category and weight_year
  weight_sums <- result[, .(total_weight = sum(weighted_consumption)), by = .(category, weight_year)]
  expect_true(all(abs(weight_sums$total_weight - 100) < 1e-6))
})

test_that("calculate_weights handles zero values correctly", {
  local_mocked_bindings(load_index_weights = function(...) {
    dt <- mock_load_index_weights(...)
    dt[1, weight := 0]
    dt
  })

  local_mocked_bindings(load_hbs = function(...) {
    dt <- mock_load_hbs(...)
    dt[1, consumption := 0]
    dt
  })

  result <- calculate_weights("FR", "income")
  expect_true(all(result$weighted_consumption > 0))
})

test_that("calculate_weights matches correct HBS year", {
  result <- calculate_weights("FR", "income")

  # Check if each weight_year is matched with the correct HBS year
  correct_matches <- result[, all(year <= weight_year), by = .(coicop, category, weight_year)]
  expect_true(all(correct_matches$V1))

  # Check if the most recent HBS year is used for each weight_year
  most_recent_matches <- result[, .SD[which.max(year)], by = .(coicop, category, weight_year)]
  expect_equal(nrow(most_recent_matches), nrow(result))
})

test_that("calculate_weights handles different date ranges", {
  result_full <- calculate_weights("FR", "income")
  result_partial <- calculate_weights("FR", "income", start_year = 2016, end_year = 2017)

  expect_lte(nrow(result_partial), nrow(result_full))
  expect_in(result_partial$weight_year, 2016:2017)
})

test_that("calculate_weights works with different categories", {
  result_income <- calculate_weights("FR", "income")
  result_age <- calculate_weights("FR", "age")
  result_urban <- calculate_weights("FR", "urban")

  expect_false(identical(result_income, result_age))
  expect_false(identical(result_income, result_urban))
  expect_false(identical(result_age, result_urban))
})

test_that("calculate_weights handles single year of data", {
  local_mocked_bindings(load_index_weights = function(...) {
    mock_load_index_weights(...)[year == 2015, ]
  })

  local_mocked_bindings(load_hbs = function(...) {
    mock_load_hbs(...)[year == 2015, ]
  })

  result <- calculate_weights("FR", "income")
  expect_true(all(result$weight_year == 2015))
  expect_true(all(result$year == 2015))
})
