create_counterfactual_sample_cpi <- function() {
  dt <- data.table::data.table(
    series_name = rep("CPI", 48),
    coicop = rep(c("01", "02"), each = 24),
    value = 100,
    year = rep(rep(2022:2023, each = 12), 2),
    month = rep(1:12, 4)
  )

  dt_basket <- data.table::data.table(
    series_name = "CPI",
    value = 100,
    year = rep(2022:2023, each = 12),
    month = rep(1:12, 2)
  )

  cpi(dt, dt_basket, "FR", 1)
}

test_that("simulate_cpi_counterfactual applies a VAT counterfactual", {
  cpi_obj <- create_counterfactual_sample_cpi()

  result <- simulate_cpi_counterfactual(
    cpi_obj,
    coicop = "01",
    type = "vat",
    start = "2022-03",
    end = "2022-08",
    old_rate = 0.21,
    new_rate = 0.06
  )

  expected <- 100 * 1.21 / 1.06
  expect_s3_class(result, "cpi")
  expect_equal(
    result$dt[coicop == "01" & year == 2022 & month == 3, value],
    expected,
    tolerance = 1e-8
  )
  expect_equal(result$dt[coicop == "01" & year == 2022 & month == 9, value], 100)
  expect_equal(result$dt[coicop == "02", value], cpi_obj$dt[coicop == "02", value])
})

test_that("simulate_cpi_counterfactual applies a tariff ratio with elasticity", {
  cpi_obj <- create_counterfactual_sample_cpi()

  result <- simulate_cpi_counterfactual(
    cpi_obj,
    coicop = "01",
    type = "ratio",
    start = "2022-04",
    end = "2022-04",
    ratio = 1.50,
    elasticity = 0.8
  )

  expect_equal(
    result$dt[coicop == "01" & year == 2022 & month == 4, value],
    100 * 1.50^0.8,
    tolerance = 1e-8
  )
})

test_that("simulate_cpi_counterfactual applies a unit subsidy counterfactual", {
  cpi_obj <- create_counterfactual_sample_cpi()

  result <- simulate_cpi_counterfactual(
    cpi_obj,
    coicop = "01",
    type = "unit_subsidy",
    start = "2022-05",
    end = "2022-06",
    subsidy = 0.20,
    unit_price = 2
  )

  expect_equal(
    result$dt[coicop == "01" & year == 2022 & month == 5, value],
    110,
    tolerance = 1e-8
  )
})

test_that("simulate_cpi_counterfactual can use an explicit counterfactual index", {
  cpi_obj <- create_counterfactual_sample_cpi()
  index <- data.table::data.table(
    date = c("2022-07", "2022-08"),
    counterfactual_index = c(125, 130)
  )

  result <- simulate_cpi_counterfactual(
    cpi_obj,
    coicop = "01",
    type = "index",
    start = "2022-07",
    end = "2022-08",
    counterfactual_index = index
  )

  expect_equal(result$dt[coicop == "01" & year == 2022 & month == 7, value], 125)
  expect_equal(result$dt[coicop == "01" & year == 2022 & month == 8, value], 130)
})

test_that("simulate_cpi_counterfactual can find the calling cpi object", {
  cpi <- create_counterfactual_sample_cpi()

  result <- simulate_cpi_counterfactual(
    coicop = "01",
    type = "ratio",
    start = "2022-02",
    end = "2022-02",
    ratio = 1.2
  )

  expect_equal(result$dt[coicop == "01" & year == 2022 & month == 2, value], 120)
})

test_that("simulate_cpi_counterfactual errors on incomplete manual inputs", {
  cpi_obj <- create_counterfactual_sample_cpi()

  expect_error(
    simulate_cpi_counterfactual(
      cpi_obj,
      coicop = "01",
      type = "vat",
      start = "2022-03",
      end = "2022-08",
      old_rate = 0.21
    ),
    "new_rate"
  )
})
