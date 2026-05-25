test_that("calculate_aggregation_bias returns gap comparison table", {
  cpi_dt <- data.table::data.table(
    series_name = rep("CPI", 24),
    coicop = rep(c("01", "011", "02", "021"), each = 6),
    value = c(
      100, 102, 104, 106, 108, 110,
      100, 103, 105, 107, 109, 112,
      100, 101, 103, 105, 106, 108,
      100, 101, 102, 104, 105, 107
    ),
    year = rep(c(2020, 2021, 2021, 2022, 2022, 2022), 4),
    month = rep(c(12, 1, 12, 1, 2, 3), 4)
  )
  cpi_basket <- data.table::data.table(
    series_name = rep("CPI", 6),
    value = c(100, 101.5, 103.5, 105.5, 107, 109),
    year = c(2020, 2021, 2021, 2022, 2022, 2022),
    month = c(12, 1, 12, 1, 2, 3)
  )
  custom_cpi <- cpi(cpi_dt, cpi_basket, country = "DE", level = 2)

  index_weights_dt <- data.table::data.table(
    coicop = rep(c("01", "011", "02", "021"), 2),
    weight = c(500, 500, 500, 500, 600, 600, 400, 400),
    year = rep(c(2021, 2022), each = 4)
  )
  custom_index_weights <- index_weights(
    index_weights_dt,
    country = "DE",
    level = 2,
    base_total = 1000
  )

  hbs_dt <- data.table::data.table(
    series_name = "HBS",
    coicop = rep(c("01", "011", "02", "021"), each = 2),
    year = 2020,
    category = rep(c("Low", "High"), 4),
    consumption = c(70, 30, 70, 30, 30, 70, 30, 70)
  )
  hbs_total <- data.table::data.table(
    series_name = "HBS",
    coicop = c("01", "011", "02", "021"),
    year = 2020,
    total_consumption = c(100, 100, 100, 100)
  )
  custom_hbs <- hbs(
    hbs_dt,
    hbs_total,
    country = "DE",
    category = "income",
    categories = c("Low", "High"),
    level = 2
  )

  result <- suppressWarnings(calculate_aggregation_bias(
    "DE", "income",
    coarse_level = 1,
    fine_level = 2,
    start_year = 2021,
    end_year = 2022,
    end_month = 3,
    custom_cpi = custom_cpi,
    custom_index_weights = custom_index_weights,
    custom_hbs = custom_hbs,
    recode_ecoicop2_to_ecoicop1 = FALSE
  ))

  expect_named(
    result,
    c(
      "country", "category_type", "coarse_level", "fine_level",
      "coarse_gap", "fine_gap", "aggregation_bias"
    )
  )
  expect_equal(result$country, "DE")
  expect_equal(result$coarse_level, 1)
  expect_equal(result$fine_level, 2)
  expect_true(is.finite(result$aggregation_bias))
  expect_equal(result$aggregation_bias, result$coarse_gap - result$fine_gap)
})
