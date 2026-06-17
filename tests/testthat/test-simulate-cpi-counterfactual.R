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

test_that("cpi_counterfactual_policy builds a monthly policy table", {
  policy <- cpi_counterfactual_policy(
    policy_id = "vat_food",
    coicop = "01",
    type = "vat",
    start = "2022-03",
    end = "2022-05",
    old_rate = 0.21,
    new_rate = 0.06
  )

  expect_s3_class(policy, "data.table")
  expect_equal(policy$policy_id, rep("vat_food", 3))
  expect_equal(policy$type, rep("vat", 3))
  expect_equal(policy$date, as.Date(c("2022-03-01", "2022-04-01", "2022-05-01")))
  expect_equal(policy$policy_ratio, rep(1.21 / 1.06, 3), tolerance = 1e-8)
  expect_true(all(is.na(policy$counterfactual_index)))
})

test_that("simulate_cpi_policy_counterfactuals applies several policies at once", {
  cpi_obj <- create_counterfactual_sample_cpi()
  policies <- data.table::rbindlist(list(
    cpi_counterfactual_policy(
      policy_id = "vat_food",
      coicop = "01",
      type = "vat",
      start = "2022-03",
      end = "2022-08",
      old_rate = 0.21,
      new_rate = 0.06
    ),
    cpi_counterfactual_policy(
      policy_id = "ratio_other",
      coicop = "02",
      type = "ratio",
      start = "2022-04",
      end = "2022-04",
      ratio = 1.5,
      elasticity = 0.8
    )
  ))

  result <- simulate_cpi_policy_counterfactuals(
    cpi_obj,
    policies,
    recalculate_price_basket = FALSE
  )

  expect_equal(
    result$dt[coicop == "01" & year == 2022 & month == 3, value],
    100 * 1.21 / 1.06,
    tolerance = 1e-8
  )
  expect_equal(
    result$dt[coicop == "02" & year == 2022 & month == 4, value],
    100 * 1.5^0.8,
    tolerance = 1e-8
  )
})

test_that("simulate_cpi_counterfactual is equivalent to the policy workflow", {
  cpi_obj <- create_counterfactual_sample_cpi()
  direct <- simulate_cpi_counterfactual(
    cpi_obj,
    coicop = "01",
    type = "vat",
    start = "2022-03",
    end = "2022-08",
    old_rate = 0.21,
    new_rate = 0.06
  )
  policy <- cpi_counterfactual_policy(
    coicop = "01",
    type = "vat",
    start = "2022-03",
    end = "2022-08",
    old_rate = 0.21,
    new_rate = 0.06
  )
  via_policy <- simulate_cpi_policy_counterfactuals(
    cpi_obj,
    policy,
    recalculate_price_basket = FALSE
  )

  expect_equal(via_policy$dt, direct$dt)
  expect_equal(via_policy$dt_basket, direct$dt_basket)
})

test_that("simulate_cpi_policy_counterfactuals recalculates the basket once", {
  cpi_obj <- create_counterfactual_sample_cpi()
  policies <- data.table::rbindlist(list(
    cpi_counterfactual_policy(
      policy_id = "vat_food",
      coicop = "01",
      type = "vat",
      start = "2023-03",
      end = "2023-03",
      old_rate = 0.21,
      new_rate = 0.06
    ),
    cpi_counterfactual_policy(
      policy_id = "ratio_other",
      coicop = "02",
      type = "ratio",
      start = "2023-03",
      end = "2023-03",
      ratio = 1.5
    )
  ))
  mock_weights <- index_weights(
    data.table::data.table(
      coicop = rep(c("01", "02"), each = 2),
      weight = 50,
      year = rep(2022:2023, 2)
    ),
    country = "FR",
    level = 1
  )
  local_mocked_bindings(
    load_index_weights = function(...) mock_weights,
    .package = "inflationinequality"
  )

  result <- simulate_cpi_policy_counterfactuals(
    cpi_obj,
    policies,
    recalculate_price_basket = TRUE
  )

  expected_item_mean <- (100 * 1.21 / 1.06 + 100 * 1.5) / 2
  expect_equal(
    result$dt_basket[year == 2023 & month == 3, value],
    expected_item_mean,
    tolerance = 1e-8
  )
})

test_that("simulate_cpi_policy_counterfactuals rejects duplicate coicop-month policies", {
  cpi_obj <- create_counterfactual_sample_cpi()
  policy <- cpi_counterfactual_policy(
    coicop = "01",
    type = "ratio",
    start = "2022-03",
    end = "2022-03",
    ratio = 1.2
  )
  policies <- data.table::rbindlist(list(policy, policy))

  expect_error(
    simulate_cpi_policy_counterfactuals(
      cpi_obj,
      policies,
      recalculate_price_basket = FALSE
    ),
    "same coicop-date"
  )
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
