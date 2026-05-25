test_that("calculate_substitution_bias returns chained Laspeyres comparison table", {
  cpi_dt <- data.table::data.table(
    series_name = rep("CPI", 12),
    coicop = rep(c("01", "02"), each = 6),
    value = c(100, 102, 104, 106, 108, 110, 100, 101, 103, 105, 106, 108),
    year = rep(c(2020, 2021, 2021, 2022, 2022, 2022), 2),
    month = rep(c(12, 1, 12, 1, 2, 3), 2)
  )
  cpi_basket <- data.table::data.table(
    series_name = rep("CPI", 6),
    value = c(100, 101.5, 103.5, 105.5, 107, 109),
    year = c(2020, 2021, 2021, 2022, 2022, 2022),
    month = c(12, 1, 12, 1, 2, 3)
  )
  custom_cpi <- cpi(cpi_dt, cpi_basket, country = "FR", level = 1)

  index_weights_dt <- data.table::data.table(
    coicop = rep(c("01", "02"), 2),
    weight = c(500, 500, 600, 400),
    year = c(2021, 2021, 2022, 2022)
  )
  custom_index_weights <- index_weights(
    index_weights_dt,
    country = "FR",
    level = 1,
    base_total = 1000
  )

  hbs_dt <- data.table::data.table(
    series_name = "HBS",
    coicop = rep(c("01", "02"), each = 2),
    year = 2020,
    category = rep(c("Low", "High"), 2),
    consumption = c(70, 30, 30, 70)
  )
  hbs_total <- data.table::data.table(
    series_name = "HBS",
    coicop = c("01", "02"),
    year = 2020,
    total_consumption = c(100, 100)
  )
  custom_hbs <- hbs(
    hbs_dt,
    hbs_total,
    country = "FR",
    category = "income",
    categories = c("Low", "High"),
    level = 1
  )

  result <- suppressWarnings(calculate_substitution_bias(
    "FR", "income",
    level = 1,
    start_year = 2021,
    end_year = 2022,
    end_month = 3,
    custom_cpi = custom_cpi,
    custom_index_weights = custom_index_weights,
    custom_hbs = custom_hbs,
    include_total = TRUE,
    recode_ecoicop2_to_ecoicop1 = FALSE
  ))

  expect_named(
    result,
    c(
      "category", "chained_laspeyres_growth",
      "chained_toernqvist_growth", "substitution_bias"
    )
  )
  expect_true(all(c("Low", "High", "Total") %in% as.character(result$category)))
  expect_true(all(is.finite(result$chained_laspeyres_growth)))
  expect_true(all(is.finite(result$chained_toernqvist_growth)))
  expect_equal(
    result$substitution_bias,
    result$chained_laspeyres_growth - result$chained_toernqvist_growth
  )
})
