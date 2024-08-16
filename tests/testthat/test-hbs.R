mock_hbs_data <- function() {
  dt <- data.table::data.table(
    series_name = c("HBS", "HBS", "HBS", NA_character_),
    coicop = c("01", "02", "01", "02"),
    year = rep(2023, 4),
    category = c("A", "A", "B", "B"),
    consumption = runif(4, 90, 110)
  )
  dt_total <- data.table::data.table(
    coicop = c("01", "02"),
    year = c(2023, 2023),
    total_consumption = c(100, 100)
  )
  country <- "FR"
  category <- "test_category"
  categories <- c("A", "B")
  level <- 1
  return(list(dt = dt, dt_total = dt_total, country = country,
              category = category, categories = categories, level = level))
}

test_that("HBS constructor works for valid arguments", {
  hbs_data <- mock_hbs_data()
  expect_no_error(
    hbs(
      hbs_data$dt, hbs_data$dt_total, hbs_data$country, hbs_data$category,
      hbs_data$categories, hbs_data$level)
  )
})

test_that("HBS object metadata is correct", {
  hbs_data <- mock_hbs_data()
  hbs_obj <- hbs(
    hbs_data$dt, hbs_data$dt_total, hbs_data$country, hbs_data$category,
    hbs_data$categories, hbs_data$level
  )
  expect_equal(hbs_obj$start_year, 2023)
  expect_equal(hbs_obj$end_year, 2023)
  expect_equal(hbs_obj$country, "FR")
  expect_equal(hbs_obj$category, "test_category")
  expect_equal(hbs_obj$categories, c("A", "B"))
  expect_equal(hbs_obj$level, 1)
})

test_that("HBS constructor fails when missing required columns", {
  hbs_data <- mock_hbs_data()
  hbs_data$dt[, coicop := NULL]
  expect_error(
    hbs(
      hbs_data$dt, hbs_data$dt_total, hbs_data$country, hbs_data$category,
      hbs_data$categories, hbs_data$level
    )
  )
})

test_that("HBS constructor fails when columns are incorrect type", {
  hbs_data <- mock_hbs_data()
  hbs_data$dt[, coicop := as.numeric(coicop)]
  expect_error(
    hbs(
      hbs_data$dt, hbs_data$dt_total, hbs_data$country, hbs_data$category,
      hbs_data$categories, hbs_data$level
    )
  )
})

test_that("HBS constructor fails with duplicate data", {
  hbs_data <- mock_hbs_data()
  hbs_data$dt <- rbind(hbs_data$dt, hbs_data$dt[1,])
  expect_error(
    hbs(
      hbs_data$dt, hbs_data$dt_total, hbs_data$country, hbs_data$category,
      hbs_data$categories, hbs_data$level
    )
  )
})

test_that("HBS constructor fails with invalid COICOP level", {
  hbs_data <- mock_hbs_data()
  hbs_data$level <- 4
  expect_error(
    hbs(
      hbs_data$dt, hbs_data$dt_total, hbs_data$country, hbs_data$category,
      hbs_data$categories, hbs_data$level
    )
  )
})

test_that("HBS constructor replaces negative consumption values with strictly positive values", {
  hbs_data <- mock_hbs_data()
  hbs_data$dt[1, consumption := -10]
  hbs <- hbs(
      hbs_data$dt, hbs_data$dt_total, hbs_data$country, hbs_data$category,
      hbs_data$categories, hbs_data$level
  )
  expect_gt(hbs$dt[1, consumption], 0)
})

test_that("HBS constructor handles missing total_consumption", {
  hbs_data <- mock_hbs_data()
  hbs_data$dt_total[1, total_consumption := NA]
  expect_message(
    hbs(
      hbs_data$dt, hbs_data$dt_total, hbs_data$country, hbs_data$category,
      hbs_data$categories, hbs_data$level
    ),
    "There are some total consumption weights that are missing"
  )
})

test_that("HBS constructor removes empty keys", {
  hbs_data <- mock_hbs_data()
  hbs_data$dt <- rbind(hbs_data$dt, data.table::data.table(
    series_name = NA_character_,
    coicop = NA_character_,
    year = NA_integer_,
    category = NA_character_,
    consumption = NA_real_
  ))
  hbs_obj <- hbs(
    hbs_data$dt, hbs_data$dt_total, hbs_data$country, hbs_data$category,
    hbs_data$categories, hbs_data$level
  )
  expect_equal(nrow(hbs_obj$dt), 4)  # The empty row should be removed
})

test_that("HBS constructor ensures strictly positive consumption", {
  hbs_data <- mock_hbs_data()
  hbs_data$dt[1, consumption := 0]
  hbs_obj <- hbs(
    hbs_data$dt, hbs_data$dt_total, hbs_data$country, hbs_data$category,
    hbs_data$categories, hbs_data$level
  )
  expect_true(all(hbs_obj$dt$consumption > 0))
})

# Helper function to create a mock HBS object with gaps
create_mock_hbs_with_gaps <- function() {
  dt <- data.table::data.table(
    series_name = c("HBS", "HBS", "HBS", "HBS"),
    coicop = c("01", "01", "02", "02"),
    year = c(2020, 2022, 2020, 2022),
    category = c("A", "A", "B", "B"),
    consumption = c(100, 200, 150, 250)
  )

  dt_total <- data.table::data.table(
    series_name = c("HBS", "HBS", "HBS", "HBS"),
    coicop = c("01", "01", "02", "02"),
    year = c(2020, 2022, 2020, 2022),
    total_consumption = c(300, 400, 350, 450)
  )

  hbs(dt = dt,
      dt_total = dt_total,
      country = "FR",
      category = "test_category",
      categories = c("A", "B"),
      level = 1)
}

test_that("interpolate_hbs fills in missing years", {
  mock_hbs <- create_mock_hbs_with_gaps()
  result <- interpolate_hbs(mock_hbs)

  expect_equal(sort(unique(result$dt$year)), 2020:2022)
  expect_equal(sort(unique(result$dt_total$year)), 2020:2022)
})

test_that("interpolate_hbs correctly interpolates values", {
  mock_hbs <- create_mock_hbs_with_gaps()
  result <- interpolate_hbs(mock_hbs)

  # Check interpolated values for dt
  expect_equal(result$dt[coicop == "01" & category == "A" & year == 2021]$consumption, 150)
  expect_equal(result$dt[coicop == "02" & category == "B" & year == 2021]$consumption, 200)

  # Check interpolated values for dt_total
  expect_equal(result$dt_total[coicop == "01" & year == 2021]$total_consumption, 350)
  expect_equal(result$dt_total[coicop == "02" & year == 2021]$total_consumption, 400)
})

test_that("interpolate_hbs preserves original data", {
  mock_hbs <- create_mock_hbs_with_gaps()
  result <- interpolate_hbs(mock_hbs)

  expect_equal(nrow(mock_hbs$dt), 4)
  expect_equal(nrow(result$dt), 6)
  expect_equal(nrow(mock_hbs$dt_total), 4)
  expect_equal(nrow(result$dt_total), 6)
})

test_that("interpolate_hbs maintains hbs object structure", {
  mock_hbs <- create_mock_hbs_with_gaps()
  result <- interpolate_hbs(mock_hbs)

  expect_s3_class(result, "hbs")
  expect_equal(result$country, mock_hbs$country)
  expect_equal(result$category, mock_hbs$category)
  expect_equal(result$categories, mock_hbs$categories)
  expect_equal(result$level, mock_hbs$level)
})

test_that("add_coicops_hbs adds missing COICOPs", {
  hbs_data <- mock_hbs_data()
  mock_hbs <- hbs(
    hbs_data$dt, hbs_data$dt_total, hbs_data$country, hbs_data$category,
    hbs_data$categories, hbs_data$level
  )
  new_coicops <- c("03", "04")

  result <- add_coicops_hbs(mock_hbs, new_coicops)

  expect_true(all(new_coicops %in% unique(result$dt$coicop)))
  expect_true(all(new_coicops %in% unique(result$dt_total$coicop)))
})

test_that("add_coicops_hbs maintains existing data", {
  hbs_data <- mock_hbs_data()
  mock_hbs <- hbs(
    hbs_data$dt, hbs_data$dt_total, hbs_data$country, hbs_data$category,
    hbs_data$categories, hbs_data$level
  )
  new_coicops <- c("03", "04")

  result <- add_coicops_hbs(mock_hbs, new_coicops)

  expect_equal(nrow(mock_hbs$dt) + length(new_coicops) * length(mock_hbs$categories), nrow(result$dt))
  expect_equal(nrow(mock_hbs$dt_total) + length(new_coicops), nrow(result$dt_total))
})

test_that("add_coicops_hbs sets default values correctly", {
  hbs_data <- mock_hbs_data()
  mock_hbs <- hbs(
    hbs_data$dt, hbs_data$dt_total, hbs_data$country, hbs_data$category,
    hbs_data$categories, hbs_data$level
  )
  new_coicops <- c("03", "04")

  result <- add_coicops_hbs(mock_hbs, new_coicops)

  new_rows <- result$dt[coicop %in% new_coicops]
  expect_true(all(new_rows$consumption == 1e-6))
  expect_true(all(is.na(new_rows$series_name)))

  new_rows_total <- result$dt_total[coicop %in% new_coicops]
  expect_true(all(new_rows_total$total_consumption == 1e-6))
  expect_true(all(is.na(new_rows_total$series_name)))
})

test_that("add_coicops_hbs handles empty input", {
  hbs_data <- mock_hbs_data()
  mock_hbs <- hbs(
    hbs_data$dt, hbs_data$dt_total, hbs_data$country, hbs_data$category,
    hbs_data$categories, hbs_data$level
  )
  new_coicops <- character(0)

  result <- add_coicops_hbs(mock_hbs, new_coicops)

  expect_equal(mock_hbs, result)
})

test_that("add_coicops_hbs handles duplicate COICOPs", {
  hbs_data <- mock_hbs_data()
  mock_hbs <- hbs(
    hbs_data$dt, hbs_data$dt_total, hbs_data$country, hbs_data$category,
    hbs_data$categories, hbs_data$level
  )
  new_coicops <- c("01", "02", "03", "03")

  expect_warning(add_coicops_hbs(mock_hbs, new_coicops))
})

test_that("add_coicops_hbs preserves original attributes", {
  hbs_data <- mock_hbs_data()
  mock_hbs <- hbs(
    hbs_data$dt, hbs_data$dt_total, hbs_data$country, hbs_data$category,
    hbs_data$categories, hbs_data$level
  )
  new_coicops <- c("03", "04")

  result <- add_coicops_hbs(mock_hbs, new_coicops)

  expect_equal(result$country, mock_hbs$country)
  expect_equal(result$category, mock_hbs$category)
  expect_equal(result$categories, mock_hbs$categories)
  expect_equal(result$level, mock_hbs$level)
  expect_equal(result$start_year, mock_hbs$start_year)
  expect_equal(result$end_year, mock_hbs$end_year)
})

test_that("add_coicops_hbs handles multi-year data correctly", {
  hbs_data <- mock_hbs_data()
  mock_hbs <- hbs(
    hbs_data$dt, hbs_data$dt_total, hbs_data$country, hbs_data$category,
    hbs_data$categories, hbs_data$level
  )
  new_coicops <- c("03", "04")

  result <- add_coicops_hbs(mock_hbs, new_coicops)

  expect_equal(length(unique(result$dt$year)), length(unique(mock_hbs$dt$year)))
  expect_true(all(table(result$dt$coicop, result$dt$year) == length(mock_hbs$categories)))
})
