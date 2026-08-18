test_that("ECOICOP v2 division 13 maps to COICOP v1 division 12 at level 1", {
  codes <- c("13", "131", "132", "133", "139", "1311", "1321", "1330", "1390")
  recoded <- inflationinequality:::recode_coicop_ecoicop2_to_ecoicop1(codes)
  level1 <- inflationinequality:::coicop_to_level(recoded, 1)

  expect_equal(level1, rep("12", length(codes)))
})

test_that("ECOICOP v2 level-2 transport and communication mappings survive level-1 truncation", {
  codes <- c("074", "0741", "0749", "082", "0821", "083", "0831", "084", "0841")
  recoded <- inflationinequality:::recode_coicop_ecoicop2_to_ecoicop1(codes)
  level1 <- inflationinequality:::coicop_to_level(recoded, 1)

  expect_equal(
    level1,
    c("08", "08", "08", "09", "08", "08", "08", "09", "09")
  )
})

test_that("ECOICOP v2 bridge applies audited manual corrections", {
  codes <- c(
    "0219", "022", "0220", "023", "0230", "024", "0240",
    "082", "0820", "0943", "0944", "0947", "0952", "09520",
    "1313", "13131", "13132"
  )
  recoded <- inflationinequality:::recode_coicop_ecoicop2_to_ecoicop1(codes)

  expect_equal(
    recoded,
    c(
      "021", "021", "021", "022", "022", "023", "023",
      "0913", "0913", "0931", "0932", "0943", "0914", "0914",
      "1211", "1211", "1211"
    )
  )
})

test_that("ECOICOP v2 bridge table documents manual corrections", {
  bridge <- data.table::as.data.table(ecoicop_v2_to_v1_bridge)
  sensitive_codes <- c("0220", "082", "0943", "0944", "0947", "0952", "1313")
  sensitive_rows <- bridge[coicop_v2 %in% sensitive_codes]

  expect_true(all(sensitive_codes %in% bridge$coicop_v2))
  expect_true(all(
    sensitive_rows$mapping_type %in%
      c("manual_choice", "manual_correction")
  ))
  expect_true(all(nzchar(sensitive_rows$note)))
})

test_that("ECOICOP v2 bridge leaves unmapped codes unchanged", {
  codes <- c("0111", "9999", NA_character_)
  recoded <- inflationinequality:::recode_coicop_ecoicop2_to_ecoicop1(codes)

  expect_equal(
    recoded,
    c("0111", "9999", NA_character_)
  )
})

test_that("ECOICOP v2 CPI recode uses lower-level HICP weights on monthly movements", {
  cpi_dt <- data.table::data.table(
    series_name = rep("CPI", 8),
    coicop = rep(c("1212", "1213", "1214", "1219"), each = 2),
    value = c(100, 110, 100, 120, 100, 130, 100, 140),
    year = rep(c(2021, 2022), 4),
    month = rep(c(12, 1), 4)
  )
  custom_cpi <- cpi(
    cpi_dt,
    data.table::data.table(
      series_name = rep("CPI", 2),
      value = c(100, 125),
      year = c(2021, 2022),
      month = c(12, 1)
    ),
    country = "FR",
    level = 3
  )
  custom_index_weights <- index_weights(
    data.table::data.table(
      coicop = c("1212", "1213", "1214", "1219"),
      weight = c(1, 2, 3, 4),
      year = 2022
    ),
    country = "FR",
    level = 3,
    base_total = 10
  )

  recoded <- inflationinequality:::recode_cpi_ecoicop2_to_ecoicop1(
    custom_cpi,
    target_level = 2,
    index_weights_obj = custom_index_weights
  )

  expect_equal(unique(recoded$dt$coicop), "125")
  recoded_dt <- data.table::copy(recoded$dt)
  recoded_dt[, date := as.Date(sprintf("%04d-%02d-01", year, month))]
  data.table::setorder(recoded_dt, coicop, date)
  recoded_dt[, dec_ratio := hicp::unchain(value, date), by = coicop]

  expect_equal(
    recoded_dt[year == 2022 & month == 1, dec_ratio],
    stats::weighted.mean(c(1.10, 1.20, 1.30, 1.40), c(1, 2, 3, 4))
  )
})

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

test_that("calculate_price_indices defaults to the package presentation base", {
  cpi_dt <- data.table::data.table(
    series_name = rep("CPI", 6),
    coicop = rep(c("01", "02"), each = 3),
    value = c(100, 102, 104, 100, 101, 103),
    year = rep(c(2024, 2025, 2025), 2),
    month = rep(c(12, 1, 2), 2)
  )
  cpi_basket <- data.table::data.table(
    series_name = rep("CPI", 3),
    value = c(100, 101.5, 103.5),
    year = c(2024, 2025, 2025),
    month = c(12, 1, 2)
  )
  custom_cpi <- cpi(cpi_dt, cpi_basket, country = "FR", level = 1)

  index_weights_dt <- data.table::data.table(
    coicop = c("01", "02"),
    weight = c(500, 500),
    year = c(2025, 2025)
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
    year = 2025,
    category = rep(c("Low", "High"), 2),
    consumption = c(70, 30, 30, 70)
  )
  hbs_total <- data.table::data.table(
    series_name = "HBS",
    coicop = c("01", "02"),
    year = 2025,
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
    start_year = 2025,
    end_year = 2025,
    end_month = 2,
    custom_cpi = custom_cpi,
    custom_index_weights = custom_index_weights,
    custom_hbs = custom_hbs
  )

  expect_equal(result$base_year, 2025)
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
  mock_cpi <- cpi(cpi_dt, cpi_basket, country = "EA", level = 2)

  index_weights_dt <- data.table::data.table(
    coicop = c("011", "012"),
    weight = c(500, 500),
    year = c(2022, 2022)
  )
  mock_index_weights <- index_weights(
    index_weights_dt,
    country = "EA",
    level = 2,
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

test_that("France INSEE level 3 HBS is available for residence area groups", {
  hbs_urban <- inflationinequality:::load_france_insee_hbs_level3(category = "urban")

  expect_true(inflationinequality:::use_france_insee_level3_hbs("FR", "urban", 3, NULL))
  expect_s3_class(hbs_urban, "hbs")
  expect_equal(hbs_urban$country, "FR")
  expect_equal(hbs_urban$category, "urban")
  expect_equal(hbs_urban$level, 3)
  expect_equal(
    hbs_urban$categories,
    c("Rural areas", "Small towns", "Medium-sized towns", "Large cities", "Paris")
  )
  expect_true("0111" %in% hbs_urban$dt$coicop)
})

test_that("calculate_price_indices uses INSEE HBS for France urban level 3", {
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
    "FR", "urban",
    level = 3,
    start_year = 2022,
    end_year = 2022,
    end_month = 3,
    custom_cpi = custom_cpi,
    custom_index_weights = custom_index_weights,
    base_year = 2022
  )

  expect_s3_class(result, "price_indices")
  expect_equal(result$category, "urban")
  expect_true(all(c("Rural areas", "Paris", "Total") %in% result$categories))
})

test_that("France INSEE level 3 HBS is available for age groups", {
  hbs_age <- inflationinequality:::load_france_insee_hbs_level3(category = "age")

  expect_true(inflationinequality:::use_france_insee_level3_hbs("FR", "age", 3, NULL))
  expect_s3_class(hbs_age, "hbs")
  expect_equal(hbs_age$country, "FR")
  expect_equal(hbs_age$category, "age")
  expect_equal(hbs_age$level, 3)
  expect_equal(
    hbs_age$categories,
    c(
      "Under 25 years", "25-34 years", "35-44 years", "45-54 years",
      "55-64 years", "65 years or over"
    )
  )
  expect_true("0111" %in% hbs_age$dt$coicop)
})

test_that("Spain EPF 2020 level 3 HBS is available for income, age, and urban groups", {
  for (category in c("income", "age", "urban")) {
    hbs_es <- inflationinequality:::load_spain_epf_2020_hbs_level3(category)

    expect_s3_class(hbs_es, "hbs")
    expect_equal(hbs_es$country, "ES")
    expect_equal(hbs_es$category, category)
    expect_equal(hbs_es$level, 3)
    expect_true("0451" %in% hbs_es$dt$coicop)
    expect_true(inflationinequality:::use_spain_epf_2020_level3_hbs("ES", category, 3, NULL))
  }
})

test_that("Portugal IDEF 2015 level 3 HBS is available for income, age, and urban groups", {
  expected_categories <- list(
    income = c(
      "First quintile", "Second quintile", "Third quintile",
      "Fourth quintile", "Fifth quintile"
    ),
    age = c(
      "Less than 30 years", "From 30 to 44 years",
      "From 45 to 59 years", "60 years or over"
    ),
    urban = c("Rural areas", "Towns and suburbs", "Cities")
  )

  for (category in names(expected_categories)) {
    hbs_pt <- inflationinequality:::load_portugal_idef_2015_hbs_level3(category)

    expect_s3_class(hbs_pt, "hbs")
    expect_equal(hbs_pt$country, "PT")
    expect_equal(hbs_pt$category, category)
    expect_equal(hbs_pt$categories, expected_categories[[category]])
    expect_equal(hbs_pt$level, 3)
    expect_true("0451" %in% hbs_pt$dt$coicop)
    expect_true(all(hbs_pt$dt$consumption > 0))
    expect_true(inflationinequality:::use_portugal_idef_2015_level3_hbs(
      "PT", category, 3, NULL
    ))
  }
})

test_that("Portugal IDEF level 3 leaves preserve published level 2 group totals", {
  hbs_pt <- inflationinequality:::load_portugal_idef_2015_hbs_level3("income")
  parents <- hbs_pt$dt[nchar(coicop) == 3L, .(
    parent_consumption = consumption
  ), by = .(category, parent = coicop)]
  leaves <- hbs_pt$dt[nchar(coicop) == 4L, .(
    leaf_consumption = sum(consumption)
  ), by = .(category, parent = substr(coicop, 1L, 3L))]
  check <- parents[leaves, on = .(category, parent), nomatch = 0L]

  expect_gt(nrow(check), 0L)
  expect_equal(check$leaf_consumption, check$parent_consumption, tolerance = 1e-5)
})

test_that("Portugal IDEF bridge retains recoded HICP parent weights", {
  hbs_pt <- inflationinequality:::load_portugal_idef_2015_hbs_level3("income")
  index_dt <- data.table::data.table(
    coicop = c("022", "112", "124", "126", "127"),
    weight = c(20, 40, 15, 10, 15),
    year = rep(2020L, 5L)
  )
  index_obj <- index_weights(
    index_dt,
    country = "PT",
    level = 3,
    base_total = 100
  )
  index_obj$ecoicop2_recoded_to_ecoicop1 <- TRUE

  result <- calculate_weights(
    "PT", "income", level = 3,
    custom_index_weights = index_obj,
    custom_hbs = hbs_pt
  )

  expect_equal(result$dt_coverage$weight_sum_avg, 100, tolerance = 1e-10)
  expected_coicops <- inflationinequality:::recode_coicop_ecoicop2_to_ecoicop1(
    index_dt$coicop
  )
  expect_setequal(unique(result$dt$coicop), expected_coicops)
  expect_true(all(result$dt_coicop_bridge$hbs_code_available))
})

test_that("calculate_price_indices uses INSEE HBS for France age level 3", {
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
    "FR", "age",
    level = 3,
    start_year = 2022,
    end_year = 2022,
    end_month = 3,
    custom_cpi = custom_cpi,
    custom_index_weights = custom_index_weights,
    base_year = 2022
  )

  expect_s3_class(result, "price_indices")
  expect_equal(result$category, "age")
  expect_true(all(c("Under 25 years", "65 years or over", "Total") %in% result$categories))
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
