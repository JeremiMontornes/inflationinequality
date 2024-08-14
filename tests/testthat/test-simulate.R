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

# Helper function to create a sample CPI object
create_sample_cpi <- function() {
  dt <- data.table::data.table(
    series_name = rep("CPI", 48),
    coicop = rep(c("01", "02", "03", "04"), each = 12),
    value = 100 + rnorm(48, mean = 0, sd = 2),
    year = rep(2022:2023, each = 24),
    month = rep(1:12, 4)
  )

  dt_basket <- data.table::data.table(
    series_name = "CPI",
    value = 100 + rnorm(24, mean = 0, sd = 1),
    year = rep(2022:2023, each = 12),
    month = rep(1:12, 2)
  )

  cpi(dt, dt_basket, "FR", 1)
}

test_that("simulate_cpi handles basic case correctly", {
  cpi_obj <- create_sample_cpi()
  simulations <- data.table::data.table(
    coicop = "01",
    shock = 10,
    start_year = 2022,
    start_month = 6,
    end_year = 2022,
    end_month = 12
  )

  result <- simulate_cpi(cpi_obj, simulations)

  expect_s3_class(result, "cpi")
  expect_true(all(result$dt[coicop == "01" & year == 2022 & month >= 6, value] >
                    cpi_obj$dt[coicop == "01" & year == 2022 & month >= 6, value]))
  expect_equal(result$dt[coicop != "01", value], cpi_obj$dt[coicop != "01", value])
})

test_that("simulate_cpi handles multiple COICOP codes", {
  cpi_obj <- create_sample_cpi()
  simulations <- data.table::data.table(
    coicop = c("01", "03"),
    shock = c(10, -5),
    start_year = c(2022, 2023),
    start_month = c(6, 1),
    end_year = c(2022, 2023),
    end_month = c(12, 6)
  )

  result <- simulate_cpi(cpi_obj, simulations)

  expect_true(all(result$dt[coicop == "01" & year == 2022 & 6 <= month & month <= 12, value] >
                    cpi_obj$dt[coicop == "01" & year == 2022 & month == 6, value]))
  expect_true(all(result$dt[coicop == "03" & year == 2023 & 1 <= month & month <= 6, value] <
                    cpi_obj$dt[coicop == "03" & year == 2023 & month == 1, value]))
})

test_that("simulate_cpi handles edge case of -100% shock", {
  cpi_obj <- create_sample_cpi()
  simulations <- data.table::data.table(
    coicop = "01",
    shock = -100,
    start_year = 2022,
    start_month = 6,
    end_year = 2022,
    end_month = 12
  )

  result <- simulate_cpi(cpi_obj, simulations)

  # Never equal to 0
  expect_true(all(result$dt[coicop == "01" & year == 2022 & month >= 6, value] == 1e-6))
})

test_that("simulate_cpi throws error for invalid shock value", {
  cpi_obj <- create_sample_cpi()
  simulations <- data.table::data.table(
    coicop = "01",
    shock = -101,
    start_year = 2022,
    start_month = 6,
    end_year = 2022,
    end_month = 12
  )

  expect_error(simulate_cpi(cpi_obj, simulations))
})

test_that("simulate_cpi handles case with no effect", {
  cpi_obj <- create_sample_cpi()
  simulations <- data.table::data.table(
    coicop = "01",
    shock = 0,
    start_year = 2022,
    start_month = 6,
    end_year = 2022,
    end_month = 12
  )

  result <- simulate_cpi(cpi_obj, simulations)

  expect_true(all(result$dt[coicop == "01" & year == 2022 & month >= 6 & month <= 12, value] ==
               result$dt[coicop == "01" & year == 2022 & month == 6, value]))
})

test_that("simulate_cpi handles case with non-existent COICOP", {
  cpi_obj <- create_sample_cpi()
  simulations <- data.table::data.table(
    coicop = "99",
    shock = 10,
    start_year = 2022,
    start_month = 6,
    end_year = 2022,
    end_month = 12
  )
  expect_warning(result <- simulate_cpi(cpi_obj, simulations), "No data found for COICOP 99")
  expect_equal(result$dt, cpi_obj$dt)
})

test_that("simulate_cpi handles case with date range outside data", {
  cpi_obj <- create_sample_cpi()
  simulations <- data.table::data.table(
    coicop = "01",
    shock = 10,
    start_year = 2024,
    start_month = 1,
    end_year = 2024,
    end_month = 12
  )

  expect_warning(result <- simulate_cpi(cpi_obj, simulations), "No data found for COICOP 01")
  expect_equal(result$dt, cpi_obj$dt)
})

test_that("simulate_cpi recalculates price basket correctly", {
  skip_if_no_internet()
  # Constant seed required because some recalculated price basket indices
  # actually decrease during the simulation period
  set.seed(0)
  dt <- data.table::CJ(
    coicop = c("01", "02"),
    year = 2022:2023,
    month = 1:12
  )
  dt <-
    dt[, .(series_name = NA_character_, coicop, year, month,
           value = 100 + rnorm(48, mean = 0, sd = 1) )]

  dt_basket <- data.table::CJ(
    year = 2022:2023,
    month = 1:12
  )
  dt_basket <-
    dt_basket[, .(series_name = NA_character_, year, month,
           value = 100 + rnorm(24, mean = 0, sd = 1) )]

  cpi_obj <- cpi(dt, dt_basket, "FR", 1)
  simulations <- data.table::data.table(
    coicop = "02",
    shock = 10,
    start_year = 2023,
    start_month = 1,
    end_year = 2023,
    end_month = 6
  )

  result <- simulate_cpi(cpi_obj, simulations, recalculate_price_basket = TRUE)

  # We see an increase in the overall price basket index
  expect_true(all(result$dt_basket[year == 2023 & month >= 1 & month <= 6, value] >= cpi_obj$dt_basket[year == 2023 & month >= 1 & month <= 6, value]))
  expect_equal(nrow(result$dt_basket), nrow(cpi_obj$dt_basket))
})

test_that("simulate_cpi handles missing parameters", {
  cpi_obj <- create_sample_cpi()
  simulations <- data.table::data.table(
    coicop = "01",
    shock = 10,
    start_year = 2022,
    start_month = 6
  )

  expect_error(simulate_cpi(cpi_obj, simulations))
})

test_that("simulate_cpi handles invalid input types", {
  cpi_obj <- create_sample_cpi()

  expect_error(simulate_cpi("not a cpi object", data.table()))
  expect_error(simulate_cpi(cpi_obj, list()))
})

test_that("simulate_cpi preserves original data outside simulation range", {
  cpi_obj <- create_sample_cpi()
  simulations <- data.table::data.table(
    coicop = "01",
    shock = 10,
    start_year = 2022,
    start_month = 6,
    end_year = 2022,
    end_month = 12
  )

  result <- simulate_cpi(cpi_obj, simulations)

  expect_equal(result$dt[year == 2023, value], cpi_obj$dt[year == 2023, value])
  expect_equal(result$dt[year == 2022 & month < 6, value], cpi_obj$dt[year == 2022 & month < 6, value])
})

test_that("simulate_cpi handles overlapping date ranges for different COICOPs", {
  cpi_obj <- create_sample_cpi()
  simulations <- data.table::data.table(
    coicop = c("01", "02"),
    shock = c(10, 5),
    start_year = c(2022, 2022),
    start_month = c(6, 9),
    end_year = c(2023, 2023),
    end_month = c(6, 3)
  )

  result <- simulate_cpi(cpi_obj, simulations)

  expect_true(all(result$dt[coicop == "01" & ((year == 2022 & month >= 6) | (year == 2023 & month <= 6)), value] >
                    cpi_obj$dt[coicop == "01" & ((year == 2022 & month >= 6) | (year == 2023 & month <= 6)), value]))
  expect_true(all(result$dt[coicop == "02" & ((year == 2022 & month >= 9) | (year == 2023 & month <= 3)), value] >
                    cpi_obj$dt[coicop == "02" & ((year == 2022 & month >= 9) | (year == 2023 & month <= 3)), value]))
})
