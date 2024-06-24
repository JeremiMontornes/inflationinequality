### load_cpi

cpi_columns <- c("series_name", "coicop", "value", "year", "month")

dt_cpi_fr <- load_cpi("FR")
dt_cpi_de <- load_cpi("DE")
dt_cpi_fr1 <- load_cpi("FR", level = 1)
dt_cpi_fr3 <- load_cpi("FR", level = 3)
dt_cpi_it_2020_2022 <- load_cpi("IT", start_year = 2020, end_year = 2022)
dt_cpi_es1_NULL_2023_5 <- load_cpi("ES", 1, end_year = 2023, end_month = 5)

test_that("CPI data formatted as data.table", {
  expect_true(data.table::is.data.table(dt_cpi_fr))
})

test_that("CPI data is non-empty", {
  expect_gt(nrow(dt_cpi_fr), 0)
})

test_that("CPI data COIcOP codes only contain digits", {
  expect_true(all(grepl('^\\d+$', dt_cpi_fr$coicop)))
})

test_that("loading French CPI data", {
  expect_equal(colnames(dt_cpi_fr), cpi_columns)
})

test_that("loading German CPI data", {
  expect_equal(colnames(dt_cpi_de), cpi_columns)
})

test_that("loading level 1 COICOP data", {
  expect_gt(nrow(dt_cpi_fr1), 0)
})

test_that("loading level 3 COICOP data", {
  expect_gt(nrow(dt_cpi_fr3), 0)
})

test_that("loading COICOP data from 2020 to 2022", {
  expect_gt(nrow(dt_cpi_it_2020_2022), 0)
  earliest_year <- min(dt_cpi_it_2020_2022$year)
  earliest_month <-  min(dt_cpi_it_2020_2022[year == earliest_year, month])
  expect_equal(earliest_year, 2020)
  expect_equal(earliest_month, 1)

  latest_year <- max(dt_cpi_it_2020_2022$year)
  latest_month <- max(dt_cpi_it_2020_2022[year == latest_year, month])
  expect_equal(latest_year, 2022)
  expect_equal(latest_month, 12)
})

test_that("loading COICOP data upto May 2023", {
  expect_gt(nrow(dt_cpi_es1_NULL_2023_5), 0)
  latest_year <- max(dt_cpi_es1_NULL_2023_5$year)
  latest_month <- max(dt_cpi_es1_NULL_2023_5[year == latest_year, month])
  expect_equal(latest_year, 2023)
  expect_equal(latest_month, 5)
})

test_that("loading end date before start date fails", {
  expect_error(load_cpi("FR", start_year = 2020, end_year = 2019))
})

# Test loading data from the beginning of time range
dt_cpi_fr_start <- load_cpi("FR", start_year = 1990, start_month = 1)
test_that("loading CPI data from start of time range", {
  expect_gt(nrow(dt_cpi_fr_start), 0)
  earliest_year <- min(dt_cpi_fr_start$year)
  earliest_month <- min(dt_cpi_fr_start[year == earliest_year, month])
  expect_equal(earliest_year, 1990)
  expect_equal(earliest_month, 1)
})

# Test loading data until the end of time range
dt_cpi_fr_end <- load_cpi("FR", end_year = 2023, end_month = 5)
test_that("loading CPI data until end of time range", {
  expect_gt(nrow(dt_cpi_fr_end), 0)
  latest_year <- max(dt_cpi_fr_end$year)
  latest_month <- max(dt_cpi_fr_end[year == latest_year, month])
  expect_equal(latest_year, 2023)
  expect_equal(latest_month, 5)
})

# Test nonexistent country code
test_that("loading CPI data with nonexistent country code", {
  expect_error(load_cpi("XX"))
})

# Test nonexistent COICOP level
test_that("loading CPI data with nonexistent COICOP level", {
  expect_error(load_cpi("FR", level = 0))
  expect_error(load_cpi("FR", level = 4))
})

# Test consistency across COICOP levels
# dt_cpi_fr_2 <- load_cpi("FR", level = 2)
# dt_cpi_fr_3 <- load_cpi("FR", level = 3)
# test_that("consistency across COICOP levels", {
#   coicop_2_digits <- unique(substr(dt_cpi_fr_3$coicop, 1, 2))
#   for (coicop_2 in coicop_2_digits) {
#     sum_level_3 <- sum(dt_cpi_fr_3[substr(coicop, 1, 2) == coicop_2, value])
#     level_2_value <- dt_cpi_fr_2[coicop == coicop_2, value]
#     expect_equal(sum_level_3, level_2_value, tolerance = 1e-6)
#   }
# })

### load_weights

weights_columns <- c("coicop", "weight", "year")

dt_weights_fr <- load_weights("FR")
dt_weights_de <- load_weights("DE")
dt_weights_fr1 <- load_weights("FR", level = 1)
dt_weights_fr3 <- load_weights("FR", level = 3)
dt_weights_it_2020_2022 <- load_weights("IT", start_year = 2020, end_year = 2022)
dt_weights_es1_NULL_2023 <- load_weights("ES", 1, end_year = 2023)

test_that("weights data formatted as data.table", {
  expect_true(data.table::is.data.table(dt_weights_fr))
})

test_that("weights data is non-empty", {
  expect_gt(nrow(dt_weights_fr), 0)
})

test_that("weights data COIcOP codes only contain digits", {
  expect_true(all(grepl('^\\d+$', dt_weights_fr$coicop)))
})

test_that("loading French weights data", {
  expect_equal(colnames(dt_weights_fr), weights_columns)
})

test_that("loading German weights data", {
  expect_equal(colnames(dt_weights_de), weights_columns)
})

test_that("loading level 1 COICOP data", {
  expect_gt(nrow(dt_weights_fr1), 0)
})

test_that("loading level 3 COICOP data", {
  expect_gt(nrow(dt_weights_fr3), 0)
})

test_that("loading COICOP data from 2020 to 2022", {
  expect_gt(nrow(dt_weights_it_2020_2022), 0)
  earliest_year <- min(dt_weights_it_2020_2022$year)
  expect_equal(earliest_year, 2020)
  latest_year <- max(dt_weights_it_2020_2022$year)
  expect_equal(latest_year, 2022)
})

test_that("loading COICOP data upto 2023", {
  expect_gt(nrow(dt_weights_es1_NULL_2023), 0)
  latest_year <- max(dt_weights_es1_NULL_2023$year)
  expect_equal(latest_year, 2023)
})

# Test loading data from the beginning of time range
dt_weights_fr_start <- load_weights("FR", start_year = 1990)
test_that("loading weights data from start of time range", {
  expect_gt(nrow(dt_weights_fr_start), 0)
  earliest_year <- min(dt_weights_fr_start$year)
  expect_ge(earliest_year, 1990)
})

# Test loading data until the end of time range
dt_weights_fr_end <- load_weights("FR", end_year = 2023)
test_that("loading weights data until end of time range", {
  expect_gt(nrow(dt_weights_fr_end), 0)
  latest_year <- max(dt_weights_fr_end$year)
  expect_le(latest_year, 2023)
})

# Test nonexistent country code
test_that("loading weights data with nonexistent country code", {
  expect_error(load_weights("XX"))
})

# Test nonexistent COICOP level
test_that("loading weights data with nonexistent COICOP level", {
  expect_error(load_weights("FR", level = 0))
  expect_error(load_weights("FR", level = 4))
})

# Test consistency across COICOP levels
# dt_weights_fr_2 <- load_weights("FR", level = 2)
# dt_weights_fr_3 <- load_weights("FR", level = 3)
# test_that("consistency across COICOP levels", {
#   coicop_2_digits <- unique(substr(dt_weights_fr_3$coicop, 1, 2))
#   for (coicop_2 in coicop_2_digits) {
#     sum_level_3 <- sum(dt_weights_fr_3[substr(coicop, 1, 2) == coicop_2, weight])
#     level_2_weight <- dt_weights_fr_2[coicop == coicop_2, weight]
#     expect_equal(sum_level_3, level_2_weight, tolerance = 1e-6)
#   }
# })

### load_hbs

hbs_columns <- c("series_name", "coicop", "year", "category", "consumption")

dt_hbs_income_fr <- load_hbs("FR", "income")
dt_hbs_age_de <- load_hbs("DE", "age")
dt_hbs_income_fr_3_2017 <- load_hbs("FR", "income", level = 3, start_year = 2017)

test_that("HBS data formatted as data.table", {
  expect_true(data.table::is.data.table(dt_hbs_income_fr))
})

test_that("HBS data.table has correct columns", {
  expect_equal(colnames(dt_hbs_income_fr), hbs_columns)
})

test_that("HBS data.table has correct COICOP level", {
  expect_true(all(nchar(dt_hbs_income_fr$coicop) == 3))
})

test_that("loading French HBS data", {
  expect_gt(nrow(dt_hbs_income_fr), 0)
  expect_false(anyNA(dt_hbs_income_fr))
})

test_that("loading German HBS data", {
  expect_gt(nrow(dt_hbs_age_de), 0)
  expect_false(anyNA(dt_hbs_age_de))
})

test_that("loading French level 3 COICOP 2017 HBS data", {
  expect_equal(colnames(dt_hbs_income_fr_3_2017), hbs_columns)
  expect_gt(nrow(dt_hbs_income_fr_3_2017), 0)
  expect_false(anyNA(dt_hbs_income_fr_3_2017))
})

# Test nonexistent country code
test_that("loading HBS data with nonexistent country code", {
  expect_error(load_hbs("XX", "income"))
})

# Test nonexistent category
test_that("loading HBS data with nonexistent category", {
  expect_error(load_hbs("FR", "nonexistent"))
})

# Test nonexistent COICOP level
test_that("loading HBS data with nonexistent COICOP level", {
  expect_error(load_hbs("FR", "income", level = 0))
  expect_error(load_hbs("FR", "income", level = 4))
})

# Test loading data from the beginning of time range
dt_hbs_fr_start <- load_hbs("FR", "income", start_year = 2000)
test_that("loading HBS data from start of time range", {
  expect_gt(nrow(dt_hbs_fr_start), 0)
  earliest_year <- min(dt_hbs_fr_start$year)
  expect_ge(earliest_year, 2000)
})

# Test loading data until the end of time range
dt_hbs_fr_end <- load_hbs("FR", "income", end_year = 2023)
test_that("loading HBS data until end of time range", {
  expect_gt(nrow(dt_hbs_fr_end), 0)
  latest_year <- max(dt_hbs_fr_end$year)
  expect_le(latest_year, 2023)
})

test_that("different income deciles for French level 3 COICOP data", {
  expect_gt(length(unique(dt_hbs_income_fr_3_2017$category)), 1)
})
