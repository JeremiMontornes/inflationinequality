test_that("build_coicop_bridge exposes exact and rolled-up mappings", {
  index_weights_dt <- data.table::data.table(
    coicop = c("01", "011", "012", "013"),
    weight = c(1000, 300, 400, 300),
    year = 2022
  )
  custom_index_weights <- index_weights(
    index_weights_dt,
    country = "FR",
    level = 2,
    base_total = 1000
  )

  hbs_dt <- data.table::data.table(
    series_name = "HBS",
    coicop = rep(c("01", "011", "012"), each = 2),
    year = 2020,
    category = rep(c("Low", "High"), 3),
    consumption = c(100, 100, 60, 40, 40, 60)
  )
  hbs_total <- data.table::data.table(
    series_name = "HBS",
    coicop = c("01", "011", "012"),
    year = 2020,
    total_consumption = c(200, 100, 100)
  )
  custom_hbs <- hbs(
    hbs_dt,
    hbs_total,
    country = "FR",
    category = "income",
    categories = c("Low", "High"),
    level = 2
  )

  bridge <- build_coicop_bridge(
    country = "FR",
    category = "income",
    level = 2,
    custom_index_weights = custom_index_weights,
    custom_hbs = custom_hbs
  )

  expect_true(data.table::is.data.table(bridge))
  expect_named(
    bridge,
    c(
      "country", "category_type", "category", "weight_year", "hbs_year",
      "hicp_coicop", "hicp_coicopv2", "hbs_coicop", "mapping_status", "hicp_weight",
      "hbs_consumption", "hbs_total_consumption", "hbs_code_available"
    )
  )
  expect_equal(bridge$hicp_coicopv2, bridge$hicp_coicop)
  expect_true(all(bridge[hicp_coicop %in% c("011", "012", "013"), hbs_coicop] == "01"))
  expect_true(all(bridge[hicp_coicop %in% c("011", "012", "013"), mapping_status] == "rolled_up_to_higher_level"))
})

test_that("write_coicop_bridge_html writes an html table", {
  bridge <- data.table::data.table(
    country = "FR",
    category_type = "income",
    category = "Low",
    weight_year = 2022,
    hbs_year = 2020,
    hicp_coicop = "011",
    hicp_coicopv2 = "011",
    hbs_coicop = "01",
    mapping_status = "rolled_up_to_higher_level",
    hicp_weight = 300,
    hbs_consumption = 100,
    hbs_total_consumption = 200,
    hbs_code_available = TRUE
  )
  out <- tempfile(fileext = ".html")

  result <- write_coicop_bridge_html(bridge, out)

  expect_equal(result, out)
  expect_true(file.exists(out))
  expect_match(paste(readLines(out), collapse = "\n"), "COICOP HICP-HBS bridge")
})

test_that("coverage is measured from HICP codes, not HBS-only codes", {
  index_weights_dt <- data.table::data.table(
    coicop = c("01", "02"),
    weight = c(600, 400),
    year = 2022
  )
  custom_index_weights <- index_weights(
    index_weights_dt,
    country = "FR",
    level = 1,
    base_total = 1000
  )

  hbs_dt <- data.table::data.table(
    series_name = "HBS",
    coicop = rep(c("01", "02", "03"), each = 2),
    year = 2020,
    category = rep(c("Low", "High"), 3),
    consumption = c(100, 100, 50, 50, 25, 25)
  )
  hbs_total <- data.table::data.table(
    series_name = "HBS",
    coicop = c("01", "02", "03"),
    year = 2020,
    total_consumption = c(200, 100, 50)
  )
  custom_hbs <- hbs(
    hbs_dt,
    hbs_total,
    country = "FR",
    category = "income",
    categories = c("Low", "High"),
    level = 1
  )

  coverage <- check_hbs_cpi_coverage(
    country = "FR",
    category = "income",
    level = 1,
    custom_index_weights = custom_index_weights,
    custom_hbs = custom_hbs,
    recode_ecoicop2_to_ecoicop1 = FALSE
  )

  expect_equal(coverage$summary$n_hicp, 2)
  expect_equal(coverage$summary$n_hbs_only, 1)
  expect_equal(coverage$summary$hicp_covered_after_rollup_rate, 1)
})
