test_that("calculate_price_indices returns chained indices by category", {
  cpi_dt <- data.table::data.table(
    series_name = rep("CPI", 8),
    coicop = rep(c("01", "02"), each = 4),
    value = c(100, 102, 104, 106, 100, 101, 103, 105),
    year = rep(c(2021, 2022, 2022, 2022), 2),
    month = rep(c(12, 1, 2, 3), 2)
  )
  cpi_basket <- data.table::data.table(
    series_name = rep("CPI", 4),
    value = c(100, 101.5, 103.5, 105.5),
    year = c(2021, 2022, 2022, 2022),
    month = c(12, 1, 2, 3)
  )
  custom_cpi <- cpi(cpi_dt, cpi_basket, country = "FR", level = 1)

  index_weights_dt <- data.table::data.table(
    coicop = c("01", "02"),
    weight = c(500, 500),
    year = c(2022, 2022)
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
    year = 2022,
    category = rep(c("Low", "High"), 2),
    consumption = c(70, 30, 30, 70)
  )
  hbs_total <- data.table::data.table(
    series_name = "HBS",
    coicop = c("01", "02"),
    year = 2022,
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

  result <- calculate_price_indices(
    "FR", "income",
    level = 1,
    start_year = 2022,
    end_year = 2022,
    end_month = 3,
    custom_cpi = custom_cpi,
    custom_index_weights = custom_index_weights,
    custom_hbs = custom_hbs,
    base_year = 2022
  )

  expect_s3_class(result, "price_indices")
  expect_named(
    result$dt,
    c("category", "year", "month", "date", "laspeyres", "chain_laspeyres", "price_index", "annual_rate")
  )
  expect_setequal(result$dt$category, c("Low", "High", "Total"))
  expect_true(all(result$dt$year == 2022))
  expect_true(all(result$dt$month %in% 1:3))
})

test_that("calculate_price_indices uses INSEE HBS for France income level 3", {
  cpi_dt <- data.table::data.table(
    series_name = rep("CPI", 8),
    coicop = rep(c("0111", "0112"), each = 4),
    value = c(100, 102, 104, 106, 100, 101, 103, 105),
    year = rep(c(2021, 2022, 2022, 2022), 2),
    month = rep(c(12, 1, 2, 3), 2)
  )
  cpi_basket <- data.table::data.table(
    series_name = rep("CPI", 4),
    value = c(100, 101.5, 103.5, 105.5),
    year = c(2021, 2022, 2022, 2022),
    month = c(12, 1, 2, 3)
  )
  custom_cpi <- cpi(cpi_dt, cpi_basket, country = "FR", level = 3)

  index_weights_dt <- data.table::data.table(
    coicop = c("0111", "0112"),
    weight = c(500, 500),
    year = c(2022, 2022)
  )
  custom_index_weights <- index_weights(
    index_weights_dt,
    country = "FR",
    level = 3,
    base_total = 1000
  )

  result <- calculate_price_indices(
    "FR", "income",
    level = 3,
    start_year = 2022,
    end_year = 2022,
    end_month = 3,
    custom_cpi = custom_cpi,
    custom_index_weights = custom_index_weights,
    base_year = 2022
  )

  expect_s3_class(result, "price_indices")
  expect_equal(result$level, 3)
  expect_true("Total" %in% result$categories)
  expect_true(any(grepl("cile1", result$categories, fixed = TRUE)))
})

test_that("calculate_price_indices uses fast euro-area total when no EA HBS is supplied", {
  cpi_dt <- data.table::data.table(
    series_name = rep("CPI", 8),
    coicop = rep(c("011", "012"), each = 4),
    value = c(100, 102, 104, 106, 100, 101, 103, 105),
    year = rep(c(2021, 2022, 2022, 2022), 2),
    month = rep(c(12, 1, 2, 3), 2)
  )
  cpi_basket <- data.table::data.table(
    series_name = rep("CPI", 4),
    value = c(100, 101.5, 103.5, 105.5),
    year = c(2021, 2022, 2022, 2022),
    month = c(12, 1, 2, 3)
  )
  mock_cpi <- cpi(cpi_dt, cpi_basket, country = "EA", level = 3)

  index_weights_dt <- data.table::data.table(
    coicop = c("011", "012"),
    weight = c(500, 500),
    year = c(2022, 2022)
  )
  mock_index_weights <- index_weights(
    index_weights_dt,
    country = "EA",
    level = 3,
    base_total = 1000
  )

  local_mocked_bindings(
    load_cpi = function(...) mock_cpi,
    load_index_weights = function(...) mock_index_weights,
    calculate_weights = function(...) stop("EA fast total should not load HBS weights"),
    .package = "inflationinequality"
  )

  result <- calculate_price_indices(
    "EA", "income",
    level = 2,
    start_year = 2022,
    end_year = 2022,
    end_month = 3,
    base_year = 2022
  )

  expect_s3_class(result, "price_indices")
  expect_equal(result$country, "EA")
  expect_equal(result$categories, "Total")
  expect_equal(unique(result$dt$category), "Total")
  expect_true(all(result$dt$month %in% 1:3))
})

test_that("France INSEE level 3 HBS can be aggregated from deciles to quintiles", {
  hbs_decile <- inflationinequality:::load_france_insee_hbs_level3("decile")
  hbs_quintile <- inflationinequality:::load_france_insee_hbs_level3("quintile")

  expect_equal(length(hbs_decile$categories), 10)
  expect_equal(
    hbs_quintile$categories,
    c("First quintile", "Second quintile", "Third quintile", "Fourth quintile", "Fifth quintile")
  )
  expect_equal(
    sort(unique(hbs_quintile$dt$coicop)),
    sort(unique(hbs_decile$dt$coicop))
  )

  sample_coicop <- "0111"
  expected_q1 <- hbs_decile$dt[
    coicop == sample_coicop & category %in% hbs_decile$categories[1:2],
    mean(consumption)
  ]
  observed_q1 <- hbs_quintile$dt[
    coicop == sample_coicop & category == "First quintile",
    consumption
  ]
  expect_equal(observed_q1, expected_q1)
  expect_equal(hbs_quintile$dt_total, hbs_decile$dt_total)
})

test_that("calculate_price_indices uses quintile option for France income level 3", {
  cpi_dt <- data.table::data.table(
    series_name = rep("CPI", 8),
    coicop = rep(c("0111", "0112"), each = 4),
    value = c(100, 102, 104, 106, 100, 101, 103, 105),
    year = rep(c(2021, 2022, 2022, 2022), 2),
    month = rep(c(12, 1, 2, 3), 2)
  )
  cpi_basket <- data.table::data.table(
    series_name = rep("CPI", 4),
    value = c(100, 101.5, 103.5, 105.5),
    year = c(2021, 2022, 2022, 2022),
    month = c(12, 1, 2, 3)
  )
  custom_cpi <- cpi(cpi_dt, cpi_basket, country = "FR", level = 3)

  index_weights_dt <- data.table::data.table(
    coicop = c("0111", "0112"),
    weight = c(500, 500),
    year = c(2022, 2022)
  )
  custom_index_weights <- index_weights(
    index_weights_dt,
    country = "FR",
    level = 3,
    base_total = 1000
  )

  result <- calculate_price_indices(
    "FR", "income",
    level = 3,
    start_year = 2022,
    end_year = 2022,
    end_month = 3,
    custom_cpi = custom_cpi,
    custom_index_weights = custom_index_weights,
    france_insee_income_groups = "quintile",
    base_year = 2022
  )

  expect_equal(
    result$categories,
    c("First quintile", "Second quintile", "Third quintile", "Fourth quintile", "Fifth quintile", "Total")
  )
  expect_false(any(grepl("cile", result$categories, fixed = TRUE)))
})
