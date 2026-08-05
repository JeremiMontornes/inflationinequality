test_that("calculate_inflation2 returns price-index-consistent year-on-year rates", {
  index_dt <- data.table::data.table(
    year = c(rep(2020L, 12), 2021L),
    month = c(1:12, 1L),
    category = "Q1",
    price_index = c(100:111, 110),
    annual_rate = c(rep(NA_real_, 12), 10)
  )
  indices <- structure(
    list(
      dt = index_dt,
      country = "XX",
      category = "income",
      categories = "Q1",
      level = 2
    ),
    class = "price_indices"
  )

  local_mocked_bindings(
    calculate_price_indices = function(...) indices,
    .package = "inflationinequality"
  )

  result <- calculate_inflation2("XX", "income")

  expect_s3_class(result, "inflation")
  expect_equal(
    result$dt,
    data.table::data.table(
      year = 2021L,
      month = 1L,
      category = "Q1",
      inflation = 100 * (110 / 100 - 1)
    )
  )
  expect_identical(result$price_indices, indices)
  expect_equal(result$start_year, 2021L)
  expect_equal(result$start_month, 1L)
})

test_that("calculate_inflation2 handles an empty annual-rate window", {
  indices <- structure(
    list(
      dt = data.table::data.table(
        year = 2020L,
        month = 1L,
        category = "Q1",
        annual_rate = NA_real_
      ),
      country = "XX",
      category = "income",
      categories = "Q1",
      level = 2
    ),
    class = "price_indices"
  )

  local_mocked_bindings(
    calculate_price_indices = function(...) indices,
    .package = "inflationinequality"
  )

  result <- calculate_inflation2("XX", "income")

  expect_equal(nrow(result$dt), 0L)
  expect_true(is.na(result$start_year))
  expect_true(is.na(result$end_month))
})
