can_reach_internet <- function() {
  tryCatch(
    {
      suppressWarnings(readLines("https://cran.r-project.org", n = 1))
      TRUE
    },
    error = function(e) FALSE
  )
}

skip_if_no_internet <- function() {
  testthat::skip_on_cran()
  if (identical(Sys.getenv("INFLATIONINEQUALITY_SKIP_NETWORK_TESTS"), "true")) {
    testthat::skip("Network tests disabled by INFLATIONINEQUALITY_SKIP_NETWORK_TESTS.")
  }
  testthat::skip_if_not(can_reach_internet(), "Internet not available.")
}

expect_inflation_matches_price_index_yoy <- function(country,
                                                     category,
                                                     level,
                                                     weighting_method,
                                                     start_year,
                                                     end_year,
                                                     start_month = NULL,
                                                     end_month = NULL,
                                                     residual_tolerance = 1e-6,
                                                     ...) {
  inflation <- suppressWarnings(suppressMessages(calculate_inflation(
    country = country,
    category = category,
    level = level,
    start_year = start_year,
    start_month = start_month,
    end_year = end_year,
    end_month = end_month,
    weighting_method = weighting_method,
    ...
  )))
  indices <- suppressWarnings(suppressMessages(calculate_price_indices(
    country = country,
    category = category,
    level = level,
    start_year = start_year,
    start_month = start_month,
    end_year = end_year,
    end_month = end_month,
    weighting_method = weighting_method,
    ...
  )))

  inflation_dt <- data.table::copy(inflation$dt)
  index_dt <- data.table::copy(indices$dt)
  index_dt <- index_dt[
    category != "Total" & !is.na(annual_rate),
    .(year, month, category, price_index_yoy = annual_rate)
  ]

  comparison <- merge(
    inflation_dt,
    index_dt,
    by = c("year", "month", "category"),
    all = FALSE
  )
  comparison[, residual := inflation - price_index_yoy]

  expect_gt(nrow(comparison), 0)
  expect_lt(max(abs(comparison$residual), na.rm = TRUE), residual_tolerance)

  invisible(comparison)
}

test_that("calculate_inflation matches price-index year-on-year rates for EA20 RAS", {
  skip_if_no_internet()

  expect_inflation_matches_price_index_yoy(
    country = "EA20",
    category = "income",
    level = 2,
    weighting_method = "ras",
    start_year = 2020,
    end_year = 2026,
    end_month = 4,
    residual_tolerance = 1e-10
  )
})

test_that("inflation and contribution calculations recode ECOICOP by default", {
  expect_true(identical(formals(calculate_inflation)$recode_ecoicop2_to_ecoicop1, TRUE))
  expect_true(identical(formals(calculate_contributions)$recode_ecoicop2_to_ecoicop1, TRUE))
})
