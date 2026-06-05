create_energy_policy_test_hbs <- function() {
  dt <- data.table::data.table(
    series_name = "HBS",
    coicop = rep(c("0451", "0111"), each = 4),
    year = 2017,
    category = rep(c("Q1", "Q2", "Q3", "Q4"), 2),
    consumption = c(100, 200, 300, 400, 900, 800, 700, 600)
  )
  dt_total <- data.table::data.table(
    coicop = rep(c("0451", "0111"), each = 1),
    year = 2017,
    total_consumption = c(250, 750)
  )
  hbs(
    dt = dt,
    dt_total = dt_total,
    country = "FR",
    category = "income",
    categories = c("Q1", "Q2", "Q3", "Q4"),
    level = 3
  )
}

test_that("simulate_energy_policy builds descriptive and policy tables", {
  res <- simulate_energy_policy(
    custom_hbs = create_energy_policy_test_hbs(),
    price_shock = 0.30,
    elasticity = -0.2,
    reference_cost_billion = 20
  )

  expect_s3_class(res, "energy_policy")
  expect_named(res$tables, c("all_households", "below_median"))
  expect_equal(nrow(res$descriptive), 4)
  expect_equal(
    res$descriptive[category == "Q1", equivalent_variation_eur],
    30
  )
  expect_equal(
    res$tables$all_households[scenario == "A1", total_cost_billion],
    20
  )
  expect_equal(
    res$tables$all_households[scenario == "A1", household_consumption_change_pct],
    6
  )
  expect_equal(
    res$tables$below_median[scenario == "A2", public_spending_top_10_pct],
    0
  )
})

test_that("simulate_energy_policy returns ggplot replications", {
  res <- simulate_energy_policy(custom_hbs = create_energy_policy_test_hbs())

  expect_s3_class(res$plots$figure6, "ggplot")
  expect_s3_class(res$plots$figure7, "ggplot")
  expect_s3_class(res$plots$figure8, "ggplot")
  expect_s3_class(res$plots$figure9, "ggplot")
})

test_that("simulate_energy_policy validates inputs", {
  bad_hbs <- create_energy_policy_test_hbs()
  bad_hbs$category <- "age"

  expect_error(
    simulate_energy_policy(custom_hbs = bad_hbs),
    "income-group"
  )
  expect_error(
    simulate_energy_policy(
      custom_hbs = create_energy_policy_test_hbs(),
      electricity_coicop = "9999"
    ),
    "not found"
  )
})

test_that("simulate_energy_policy can use the compact Spain EPF 2020 data", {
  spain_hbs <- inflationinequality:::load_spain_epf_2020_hbs_if_available()
  testthat::skip_if(
    is.null(spain_hbs),
    "Compact Spain EPF 2020 HBS data not built locally."
  )

  res <- simulate_energy_policy(country = "ES")

  expect_s3_class(res, "energy_policy")
  expect_identical(res$metadata$hbs_country, "ES")
  expect_true(all(res$metadata$hbs_years == 2020))
  expect_equal(nrow(res$descriptive), 10)
  expect_true(all(c("A1", "B1", "C1") %in% res$tables$all_households$scenario))
})
