exclusion_fixture <- function() {
  codes <- c("011", "012", "031", "041", "042", "045")
  categories <- c("First quintile", "Second quintile", "Third quintile", "Fourth quintile", "Fifth quintile")
  p <- data.table::CJ(coicop = setdiff(codes, "042"), year = 2019:2023, month = 1:12)
  p[, value := 100 * (1 + match(coicop, codes) / 1000)^((year - 2019) * 12 + month)]
  p[, series_name := "test"]
  iw <- data.table::CJ(coicop = setdiff(codes, "042"), year = 2020:2023)
  iw[, weight := 200]
  hd <- data.table::CJ(coicop = codes, category = categories)
  hd[, `:=`(year = 2020L, series_name = "test",
            consumption = 10 + match(coicop, codes) * match(category, categories))]
  ht <- hd[, .(series_name = "test", total_consumption = mean(consumption)), by = .(coicop, year)]
  list(custom_cpi = structure(list(dt = p, country = "XX", level = 2,
      start_year = 2019, end_year = 2023), class = "cpi"),
    custom_index_weights = index_weights(iw, country = "XX", level = 2, base_total = 1000),
    custom_hbs = hbs(hd, ht, country = "XX", category = "income", categories = categories, level = 2))
}

test_that("exclusion matches manual filtering including the total and does not mutate inputs", {
  f <- exclusion_fixture()
  before <- data.table::copy(f)
  args <- c(list(country = "XX", category = "income", level = 2, start_year = 2020,
    end_year = 2023, base_year = 2020, include_total = TRUE,
    recode_ecoicop2_to_ecoicop1 = FALSE), f)
  result <- do.call(calculate_inflation2, c(args, list(exclude_coicop = c("041", "042"))))
  manual <- f
  manual$custom_hbs$dt <- manual$custom_hbs$dt[!coicop %in% c("041", "042")]
  manual$custom_hbs$dt_total <- manual$custom_hbs$dt_total[!coicop %in% c("041", "042")]
  manual$custom_index_weights$dt <- manual$custom_index_weights$dt[!coicop %in% c("041", "042")]
  manual$custom_cpi$dt <- manual$custom_cpi$dt[!coicop %in% c("041", "042")]
  manual_args <- args
  manual_args[names(manual)] <- manual
  reference <- do.call(calculate_inflation2, manual_args)
  expect_equal(result$dt, reference$dt, tolerance = 1e-10)
  expect_equal(result$price_indices$dt, reference$price_indices$dt, tolerance = 1e-10)
  expect_identical(result$exclude_coicop, c("041", "042"))
  for (name in names(f)) expect_identical(as.data.frame(f[[name]]$dt), as.data.frame(before[[name]]$dt))
  expect_identical(as.data.frame(f$custom_hbs$dt_total), as.data.frame(before$custom_hbs$dt_total))
  ew <- result$price_indices$dt_effective_weights
  expect_false(any(grepl("^04[12]", ew$coicop)))
  expect_equal(do.call(calculate_inflation2, args)$dt,
    do.call(calculate_inflation2, c(args, list(exclude_coicop = NULL)))$dt)
})

test_that("exclusion precedes the rent bridge and weights normalize", {
  local_mocked_bindings(ras_category_shares = function(hbs_category, country, categories, weight_years) {
    data.table::CJ(category = categories, weight_year = weight_years)[, category_share := 1 / length(categories)]
  }, .package = "inflationinequality")
  f <- exclusion_fixture()
  args <- c(list(country = "XX", category = "income", level = 2),
    f[c("custom_hbs", "custom_index_weights")])
  result <- do.call(calculate_weights, c(args, list(exclude_coicop = "042")))
  expect_false(any(result$dt_coicop_bridge$mapping_status == "combined_hbs_041_042"))
  manual_hbs <- f$custom_hbs
  manual_hbs$dt <- manual_hbs$dt[coicop != "042"]
  manual_hbs$dt_total <- manual_hbs$dt_total[coicop != "042"]
  manual_iw <- f$custom_index_weights
  manual_iw$dt <- manual_iw$dt[coicop != "042"]
  reference <- calculate_weights("XX", "income", level = 2,
    custom_hbs = manual_hbs, custom_index_weights = manual_iw)
  expect_equal(result$dt, reference$dt)
  for (method in c("relative_expenditure", "ras", "additive_qp")) {
    z <- do.call(calculate_weights, c(args, list(exclude_coicop = "04", weighting_method = method)))
    expect_false(any(startsWith(z$dt$coicop, "04")))
    expect_equal(z$dt[, sum(weighted_consumption), by = .(category, weight_year)]$V1, rep(100, 20), tolerance = 1e-8)
  }
})

test_that("prefix exclusions reject invalid or unresolvable specifications", {
  expect_identical(coicop_is_excluded(c("041", "0411", "042", "043"), c("041", "042")), c(TRUE, TRUE, TRUE, FALSE))
  expect_error(calculate_inflation2("XX", "income", exclude_coicop = 41), "character")
  expect_error(calculate_inflation2("XX", "income", exclude_coicop = NA_character_), "character")
  expect_error(calculate_inflation2("XX", "income", level = 1, exclude_coicop = "041"), "finer")
  f <- exclusion_fixture()
  expect_error(do.call(calculate_inflation2, c(list(country = "XX", category = "income", level = 2,
    exclude_coicop = c("01", "03", "04"), recode_ecoicop2_to_ecoicop1 = FALSE), f)), "removes all")
})

test_that("multi-country dispatch forwards exclusions to national indices", {
  f <- exclusion_fixture()
  local_mocked_bindings(
    load_cpi = function(...) f$custom_cpi,
    load_index_weights = function(...) f$custom_index_weights,
    load_hbs = function(...) f$custom_hbs,
    .package = "inflationinequality")
  cw <- data.table::CJ(country = c("XX", "YY"), year = 2020:2023)[, weight := 1]
  result <- calculate_inflation2(c("XX", "YY"), "income", level = 2,
    start_year = 2020, end_year = 2023, base_year = 2020,
    custom_country_weights = cw, recode_ecoicop2_to_ecoicop1 = FALSE,
    exclude_coicop = c("041", "042"))
  reference <- do.call(calculate_inflation2, c(list(country = "XX", category = "income", level = 2,
    start_year = 2020, end_year = 2023, base_year = 2020,
    recode_ecoicop2_to_ecoicop1 = FALSE, exclude_coicop = c("041", "042")), f))
  expect_equal(result$dt, reference$dt, tolerance = 1e-9)
  expect_identical(result$price_indices$exclude_coicop, c("041", "042"))
})

test_that("parent fallback cannot reintroduce excluded HBS expenditure", {
  f <- exclusion_fixture()
  parent <- f$custom_hbs$dt[startsWith(coicop, "04"),
    .(series_name = "test", coicop = "04", consumption = sum(consumption)), by = .(year, category)]
  totals <- f$custom_hbs$dt_total[startsWith(coicop, "04"),
    .(series_name = "test", coicop = "04", total_consumption = sum(total_consumption)), by = year]
  f$custom_hbs$dt <- data.table::rbindlist(list(f$custom_hbs$dt[coicop != "045"], parent), use.names = TRUE)
  f$custom_hbs$dt_total <- data.table::rbindlist(list(f$custom_hbs$dt_total[coicop != "045"], totals), use.names = TRUE)
  expect_error(calculate_weights("XX", "income", level = 2,
    custom_hbs = f$custom_hbs, custom_index_weights = f$custom_index_weights,
    exclude_coicop = c("041", "042")), "parent containing excluded")
})
