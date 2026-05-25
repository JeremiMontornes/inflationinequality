test_that("aggregate_price_indices_by_country computes weighted country means", {
  dates <- as.Date(c("2020-01-01", "2021-01-01"))
  make_indices <- function(price_index) {
    structure(
      list(
        dt = data.table::data.table(
          category = rep(c("First quintile", "Total"), each = 2),
          year = rep(c(2020, 2021), 2),
          month = 1L,
          date = rep(dates, 2),
          laspeyres = price_index / 100,
          chain_laspeyres = price_index / 100,
          price_index = price_index,
          annual_rate = NA_real_
        ),
        country = "XX",
        category = "income",
        categories = c("First quintile", "Total"),
        level = 2,
        base_year = 2020,
        formula = "laspeyres"
      ),
      class = "price_indices"
    )
  }

  de <- make_indices(c(100, 110, 100, 108))
  fr <- make_indices(c(100, 120, 100, 112))
  out <- aggregate_price_indices_by_country(
    list(DE = de, FR = fr),
    country_weights = data.table::data.table(
      country = rep(c("DE", "FR"), each = 2),
      year = rep(c(2020, 2021), 2),
      weight = c(3, 3, 1, 1)
    ),
    aggregate_geo = "EA20",
    category = "income",
    level = 2,
    base_year = 2020
  )

  expect_s3_class(out, "price_indices")
  expect_equal(out$country, "EA20")
  expect_equal(out$source_countries, c("DE", "FR"))
  expect_equal(
    out$dt[category == "First quintile" & year == 2021, price_index],
    112.5
  )
  expect_equal(
    out$dt[category == "Total" & year == 2021, price_index],
    109
  )
})

test_that("aggregate_price_indices_by_country requires complete weights", {
  dates <- as.Date("2020-01-01")
  indices <- structure(
    list(
      dt = data.table::data.table(
        category = "Total",
        year = 2020L,
        month = 1L,
        date = dates,
        laspeyres = 1,
        chain_laspeyres = 1,
        price_index = 100,
        annual_rate = NA_real_
      ),
      country = "DE",
      category = "income",
      categories = "Total",
      level = 2,
      base_year = 2020,
      formula = "laspeyres"
    ),
    class = "price_indices"
  )

  expect_error(
    aggregate_price_indices_by_country(
      list(DE = indices, FR = indices),
      country_weights = data.table::data.table(
        country = "DE",
        year = 2020,
        weight = 1
      )
    ),
    "Missing country weights"
  )
})
