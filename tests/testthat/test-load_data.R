### load_cpi

cpi_columns <- c("series_name", "coicop", "value", "year", "month")

load_cpi <<- memoise::memoise(load_cpi)

cpi_fr <- load_cpi("FR")
cpi_de <- load_cpi("DE")
cpi_fr1 <- load_cpi("FR", level = 1)
cpi_fr3 <- load_cpi("FR", level = 3)
cpi_it_2020_2022 <- load_cpi("IT", start_year = 2020, end_year = 2022)
cpi_es1_NULL_2023_5 <- load_cpi("ES", 1, end_year = 2023, end_month = 5)

test_that("CPI data formatted as data.table", {
  expect_s3_class(cpi_fr, "cpi")
  expect_true(data.table::is.data.table(cpi_fr$dt))
})

test_that("CPI data is non-empty", {
  expect_gt(nrow(cpi_fr$dt), 0)
})

test_that("CPI data COIcOP codes only contain digits", {
  expect_true(all(grepl("^\\d+$", cpi_fr$dt$coicop)))
})

test_that("loading French CPI data", {
  expect_equal(colnames(cpi_fr$dt), cpi_columns)
})

test_that("loading German CPI data", {
  expect_equal(colnames(cpi_de$dt), cpi_columns)
})

test_that("loading level 1 COICOP data", {
  expect_gt(nrow(cpi_fr1$dt), 0)
})

test_that("loading level 3 COICOP data", {
  expect_gt(nrow(cpi_fr3$dt), 0)
})

test_that("loading COICOP data from 2020 to 2022", {
  expect_gt(nrow(cpi_it_2020_2022$dt), 0)
  earliest_year <- min(cpi_it_2020_2022$dt$year)
  earliest_month <- min(cpi_it_2020_2022$dt[year == earliest_year, month])
  expect_equal(earliest_year, 2020)
  expect_equal(earliest_month, 1)

  latest_year <- max(cpi_it_2020_2022$dt$year)
  latest_month <- max(cpi_it_2020_2022$dt[year == latest_year, month])
  expect_equal(latest_year, 2022)
  expect_equal(latest_month, 12)
})

test_that("loading COICOP data upto May 2023", {
  expect_gt(nrow(cpi_es1_NULL_2023_5$dt), 0)
  latest_year <- max(cpi_es1_NULL_2023_5$dt$year)
  latest_month <- max(cpi_es1_NULL_2023_5$dt[year == latest_year, month])
  expect_equal(latest_year, 2023)
  expect_equal(latest_month, 5)
})

test_that("loading end date before start date fails", {
  expect_error(load_cpi("FR", start_year = 2020, end_year = 2019))
})

# Test loading data from the beginning of time range
cpi_fr_start <- load_cpi("FR", start_year = 1990, start_month = 1)
test_that("loading CPI data from start of time range", {
  expect_gt(nrow(cpi_fr_start$dt), 0)
  earliest_year <- min(cpi_fr_start$dt$year)
  earliest_month <- min(cpi_fr_start$dt[year == earliest_year, month])
  expect_gte(earliest_year, 1990)
  expect_equal(earliest_month, 1)
})

# Test loading data until the end of time range
cpi_fr_end <- load_cpi("FR", end_year = 2023, end_month = 5)
test_that("loading CPI data until end of time range", {
  expect_gt(nrow(cpi_fr_end$dt), 0)
  latest_year <- max(cpi_fr_end$dt$year)
  latest_month <- max(cpi_fr_end$dt[year == latest_year, month])
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
# cpi_fr$dt_2 <- load_cpi("FR", level = 2)
# cpi_fr$dt_3 <- load_cpi("FR", level = 3)
# test_that("consistency across COICOP levels", {
#   coicop_2_digits <- unique(substr(cpi_fr$dt_3$coicop, 1, 2))
#   for (coicop_2 in coicop_2_digits) {
#     sum_level_3 <- sum(cpi_fr$dt_3[substr(coicop, 1, 2) == coicop_2, value])
#     level_2_value <- cpi_fr$dt_2[coicop == coicop_2, value]
#     expect_equal(sum_level_3, level_2_value, tolerance = 1e-6)
#   }
# })

### load_index_weights

weights_columns <- c("coicop", "weight", "year")

weights_fr <- load_index_weights("FR")
weights_de <- load_index_weights("DE")
weights_fr1 <- load_index_weights("FR", level = 1)
weights_fr3 <- load_index_weights("FR", level = 3)
weights_it_2020_2022 <- load_index_weights("IT", start_year = 2020, end_year = 2022)
weights_es1_NULL_2023 <- load_index_weights("ES", 1, end_year = 2023)

test_that("weights data formatted as data.table", {
  expect_s3_class(weights_fr, "index_weights")
  expect_true(data.table::is.data.table(weights_fr$dt))
})

test_that("weights data is non-empty", {
  expect_gt(nrow(weights_fr$dt), 0)
})

test_that("weights data COIcOP codes only contain digits", {
  expect_true(all(grepl("^\\d+$", weights_fr$dt$coicop)))
})

test_that("loading French weights data", {
  expect_equal(colnames(weights_fr$dt), weights_columns)
})

test_that("loading German weights data", {
  expect_equal(colnames(weights_de$dt), weights_columns)
})

test_that("loading level 1 COICOP data", {
  expect_gt(nrow(weights_fr1$dt), 0)
})

test_that("loading level 3 COICOP data", {
  expect_gt(nrow(weights_fr3$dt), 0)
})

test_that("loading COICOP data from 2020 to 2022", {
  expect_gt(nrow(weights_it_2020_2022$dt), 0)
  earliest_year <- min(weights_it_2020_2022$dt$year)
  expect_equal(earliest_year, 2020)
  latest_year <- max(weights_it_2020_2022$dt$year)
  expect_equal(latest_year, 2022)
})

test_that("loading COICOP data upto 2023", {
  expect_gt(nrow(weights_es1_NULL_2023$dt), 0)
  latest_year <- max(weights_es1_NULL_2023$dt$year)
  expect_equal(latest_year, 2023)
})

# Test loading data from the beginning of time range
weights_fr_start <- load_index_weights("FR", start_year = 1990)
test_that("loading weights data from start of time range", {
  expect_gt(nrow(weights_fr_start$dt), 0)
  earliest_year <- min(weights_fr_start$dt$year)
  expect_gte(earliest_year, 1990)
})

# Test loading data until the end of time range
weights_fr_end <- load_index_weights("FR", end_year = 2023)
test_that("loading weights data until end of time range", {
  expect_gt(nrow(weights_fr_end$dt), 0)
  latest_year <- max(weights_fr_end$dt$year)
  expect_lte(latest_year, 2023)
})

# Test nonexistent country code
test_that("loading weights data with nonexistent country code", {
  expect_error(load_index_weights("XX"))
})

# Test nonexistent COICOP level
test_that("loading weights data with nonexistent COICOP level", {
  expect_error(load_index_weights("FR", level = 0))
  expect_error(load_index_weights("FR", level = 4))
})

# Test consistency across COICOP levels
# weights_fr$dt_2 <- load_index_weights("FR", level = 2)
# weights_fr$dt_3 <- load_index_weights("FR", level = 3)
# test_that("consistency across COICOP levels", {
#   coicop_2_digits <- unique(substr(weights_fr$dt_3$coicop, 1, 2))
#   for (coicop_2 in coicop_2_digits) {
#     sum_level_3 <- sum(weights_fr$dt_3[substr(coicop, 1, 2) == coicop_2, weight])
#     level_2_weight <- weights_fr$dt_2[coicop == coicop_2, weight]
#     expect_equal(sum_level_3, level_2_weight, tolerance = 1e-6)
#   }
# })

### load_hbs

hbs_columns <- c("series_name", "coicop", "year", "category", "consumption")

hbs_income_fr <- load_hbs("FR", "income")
hbs_age_de <- load_hbs("DE", "age")
hbs_income_fr_3_2017 <- load_hbs("FR", "income", level = 3, start_year = 2017)

test_that("HBS data formatted as data.table", {
  expect_s3_class(hbs_income_fr, "hbs")
  expect_true(data.table::is.data.table(hbs_income_fr$dt))
})

test_that("HBS data.table has correct columns", {
  expect_equal(colnames(hbs_income_fr$dt), hbs_columns)
})

test_that("HBS data.table has correct COICOP level", {
  expect_true(all(nchar(hbs_income_fr$dt$coicop) == 3))
})

test_that("loading French HBS data", {
  expect_gt(nrow(hbs_income_fr$dt), 0)
  expect_false(anyNA(hbs_income_fr$dt))
})

test_that("loading German HBS data", {
  expect_gt(nrow(hbs_age_de$dt), 0)
  expect_false(anyNA(hbs_age_de$dt))
})

test_that("loading French level 3 COICOP 2017 HBS data", {
  expect_equal(colnames(hbs_income_fr_3_2017$dt), hbs_columns)
  expect_gt(nrow(hbs_income_fr_3_2017$dt), 0)
  expect_false(anyNA(hbs_income_fr_3_2017$dt))
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
hbs_fr_start <- load_hbs("FR", "income", start_year = 2000)
test_that("loading HBS data from start of time range", {
  expect_gt(nrow(hbs_fr_start$dt), 0)
  earliest_year <- min(hbs_fr_start$dt$year)
  expect_gte(earliest_year, 2000)
})

# Test loading data until the end of time range
hbs_fr_end <- load_hbs("FR", "income", end_year = 2023)
test_that("loading HBS data until end of time range", {
  expect_gt(nrow(hbs_fr_end$dt), 0)
  latest_year <- max(hbs_fr_end$dt$year)
  expect_lte(latest_year, 2023)
})

test_that("different income deciles for French level 3 COICOP data", {
  expect_gt(length(unique(hbs_income_fr_3_2017$dt$category)), 1)
})
