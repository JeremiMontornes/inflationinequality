test_that("compare_to_official_hicp validates calculated average against basket HICP", {
  inflation_dt <- data.table::data.table(
    year = rep(2022, 6),
    month = rep(1:3, each = 2),
    category = rep(c("Low", "High"), 3),
    inflation = c(4, 6, 5, 7, 6, 8)
  )
  inflation <- structure(
    list(
      dt = inflation_dt,
      country = "FR",
      category = "income",
      categories = c("Low", "High"),
      start_year = 2022,
      start_month = 1,
      end_year = 2022,
      end_month = 3
    ),
    class = "inflation"
  )

  months <- seq.Date(as.Date("2021-01-01"), as.Date("2022-03-01"), by = "month")
  cpi_dt <- data.table::data.table(
    series_name = "HICP",
    coicop = "01",
    value = 100,
    year = as.integer(format(months, "%Y")),
    month = as.integer(format(months, "%m"))
  )
  cpi_basket <- data.table::data.table(
    series_name = "HICP",
    value = c(rep(100, 12), 105, 106, 107),
    year = as.integer(format(months, "%Y")),
    month = as.integer(format(months, "%m"))
  )
  custom_cpi <- cpi(cpi_dt, cpi_basket, country = "FR", level = 1)

  result <- compare_to_official_hicp(inflation, custom_cpi = custom_cpi)

  expect_s3_class(result, "hicp_comparison")
  expect_named(result, c("dt", "summary", "plot", "country", "measure"))
  expect_named(
    result$dt,
    c("year", "month", "date", "calculated_value", "official_value", "difference")
  )
  expect_equal(result$dt$calculated_value, c(5, 6, 7))
  expect_equal(round(result$dt$official_value, 6), c(5, 6, 7))
  expect_equal(result$dt$difference, c(0, 0, 0), tolerance = 1e-10)
  expect_s3_class(result$plot, "ggplot")
})

test_that("compare_to_official_hicp compares price index levels", {
  months <- seq.Date(as.Date("2021-01-01"), as.Date("2022-03-01"), by = "month")
  cpi_dt <- data.table::data.table(
    series_name = "HICP",
    coicop = "01",
    value = 100,
    year = as.integer(format(months, "%Y")),
    month = as.integer(format(months, "%m"))
  )
  cpi_basket <- data.table::data.table(
    series_name = "HICP",
    value = c(rep(100, 12), 105, 106, 107),
    year = as.integer(format(months, "%Y")),
    month = as.integer(format(months, "%m"))
  )
  custom_cpi <- cpi(cpi_dt, cpi_basket, country = "FR", level = 1)

  indices_dt <- data.table::data.table(
    category = rep(c("Low", "High", "Total"), each = 3),
    year = rep(2022, 9),
    month = rep(1:3, 3),
    date = rep(as.Date(c("2022-01-01", "2022-02-01", "2022-03-01")), 3),
    laspeyres = 1,
    chain_laspeyres = 1,
    price_index = c(104, 105, 106, 106, 107, 108, 105, 106, 107),
    annual_rate = NA_real_
  )
  indices <- structure(
    list(
      dt = indices_dt,
      country = "FR",
      category = "income",
      categories = c("Low", "High", "Total"),
      level = 1,
      start_year = 2022,
      start_month = 1,
      end_year = 2022,
      end_month = 3,
      base_year = 2021
    ),
    class = "price_indices"
  )

  result <- NULL
  expect_warning(
    result <- compare_to_official_hicp(indices, custom_cpi = custom_cpi),
    "base_year=2021"
  )

  expect_equal(result$measure, "level")
  expect_equal(result$dt$calculated_value, c(105, 106, 107))
  expect_equal(round(result$dt$official_value, 6), c(105, 106, 107))
  expect_equal(result$dt$difference, c(0, 0, 0), tolerance = 1e-10)
  expect_s3_class(result$plot, "ggplot")
})
