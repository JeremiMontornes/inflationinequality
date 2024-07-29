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

mock_cpi_data <- function() {
  dt <- data.table::data.table(
    series_name = c("CPI", "CPI", "CPI", NA_character_),
    coicop = c("01", "02", "01", "02"),
    value = runif(4, 90, 110),
    year = rep(2023, 4),
    month = c(9, 9, 10, 10)
  )
  dt_basket <- data.table::data.table(
    series_name = c("CPI", NA_character_),
    value = c(100, 100),
    year = c(2023, 2023),
    month = c(9, 10)
  )
  country <- "FR"
  level <- 1
  return(list(dt = dt, dt_basket = dt_basket, country = country, level = level))
}

test_that("CPI constructor works for valid arguments", {
  cpi_data <- mock_cpi_data()
  expect_no_error(cpi(cpi_data$dt, cpi_data$dt_basket, cpi_data$country, cpi_data$level))
})

test_that("CPI object metadata is correct", {
  cpi_data <- mock_cpi_data()
  cpi <- cpi(cpi_data$dt, cpi_data$dt_basket, cpi_data$country, cpi_data$level)
  expect_equal(cpi$start_year, 2023)
  expect_equal(cpi$end_year, 2023)
  expect_equal(cpi$start_month, 9)
  expect_equal(cpi$end_month, 10)
})

test_that("CPI constructor fails when missing required columns", {
  cpi_data <- mock_cpi_data()
  cpi_data$dt[, series_name := NULL]
  expect_error(cpi(cpi_data$dt, cpi_data$dt_basket, cpi_data$country, cpi_data$level))
})

test_that("CPI constructor fails when columns are incorrect type", {
  cpi_data <- mock_cpi_data()
  cpi_data$dt[, coicop := c(1, 2, 1, 2)]
  expect_error(cpi(cpi_data$dt, cpi_data$dt_basket, cpi_data$country, cpi_data$level))
})

test_that("CPI constructor fails when missing basket data", {
  cpi_data <- mock_cpi_data()
  cpi_data$dt_basket <- data.table::data.table(
    series_name = c("CPI"),
    value = c(100),
    year = c(2023),
    month = c(9)
  )
  expect_error(cpi(cpi_data$dt, cpi_data$dt_basket, cpi_data$country, cpi_data$level))
})

test_that("CPI constructor throws warning when basket data has more data", {
  cpi_data <- mock_cpi_data()
  cpi_data$dt_basket <- data.table::data.table(
    series_name = c("CPI", NA_character_, NA_character_),
    value = c(100, 100, 100),
    year = c(2023, 2023, 2023),
    month = c(9, 10, 11)
  )
  expect_warning(cpi(cpi_data$dt, cpi_data$dt_basket, cpi_data$country, cpi_data$level))
})

test_that("CPI constructor fails with duplicate data", {
  cpi_data <- mock_cpi_data()
  cpi_data$dt <- data.table::data.table(
    series_name = c("CPI", "CPI", "CPI", NA_character_),
    coicop = c("01", "02", "01", "01"),
    value = runif(4, 90, 110),
    year = rep(2023, 4),
    month = c(9, 9, 10, 10)
  )
  expect_error(cpi(cpi_data$dt, cpi_data$dt_basket, cpi_data$country, cpi_data$level))
})

test_that("CPI constructor fails with duplicate basket data", {
  cpi_data <- mock_cpi_data()
  cpi_data$dt_basket <- data.table::data.table(
    series_name = c("CPI", NA_character_),
    value = c(100, 100),
    year = c(2023, 2023),
    month = c(9, 9)
  )
  expect_error(cpi(cpi_data$dt, cpi_data$dt_basket, cpi_data$country, cpi_data$level))
})

test_that("get_missing_cpi_tuples works", {
  cpi_data <- mock_cpi_data()
  cpi_data$dt <- data.table::data.table(
    series_name = c("CPI", "CPI", NA_character_),
    coicop = c("02", "01", "02"),
    value = runif(3, 90, 110),
    year = rep(2023, 3),
    month = c(9, 10, 10)
  )
  cpi <- cpi(cpi_data$dt, cpi_data$dt_basket, cpi_data$country, cpi_data$level)
  missing_cpi_tuples <- get_missing_cpi_tuples(cpi)
  dt_expected <- data.table::data.table(
    coicop = "01",
    year = 2023,
    month = 9
  )
  expect_equal(missing_cpi_tuples, dt_expected, ignore_attr = TRUE)
})

test_that("correct_cpi fails at level 1 COICOP", {
  cpi_data <- mock_cpi_data()
  cpi <- cpi(cpi_data$dt, cpi_data$dt_basket, cpi_data$country, cpi_data$level)
  expect_error(correct_cpi(cpi))
})

test_that("correct_cpi works", {
  skip_if_no_internet()
  dt <- data.table::data.table(
    series_name = rep(NA_character_, 2 * 12 + 1),
    coicop = c(rep("011", 12), rep("012", 12), "011"),
    value = runif(2 * 12 + 1, 90, 110),
    year = c(rep(2023, 2 * 12), 2022),
    month = c(rep(1:12, 2), 12)
  )
  dt_basket <- data.table::data.table(
    series_name = rep(NA_character_, 12 + 1),
    value = runif(12 + 1, 90, 110),
    year = c(rep(2023, 12), 2022),
    month = c(1:12, 12)
  )
  country <- "FR"
  level <- 2
  cpi <- cpi(dt, dt_basket, country, level)

  missing_coicops <- get_missing_cpi_tuples(cpi)
  expect_equal(nrow(missing_coicops), 1)

  corrected_cpi <- correct_cpi(cpi)
  corrected_coicops <- get_missing_cpi_tuples(corrected_cpi)
  expect_equal(nrow(corrected_coicops), 0)
})

test_that("correct_cpi throws warning for not having enough data", {
  skip_if_no_internet()
  dt <- data.table::data.table(
    series_name = rep(NA_character_, 3),
    coicop = c("011", "011", "012"),
    value = runif(3, 90, 110),
    year = rep(2023, 3),
    month = c(9, 10, 10)
  )
  dt_basket <- data.table::data.table(
    series_name = rep(NA_character_, 2),
    value = c(100, 100),
    year = c(2023, 2023),
    month = c(9, 10)
  )
  country <- "FR"
  level <- 2
  cpi <- cpi(dt, dt_basket, country, level)
  expect_warning(correct_cpi(cpi))
})
