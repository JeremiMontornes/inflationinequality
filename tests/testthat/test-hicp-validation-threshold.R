test_that("level-2 RAS total inflation tracks published HICP for DE and FR", {
  testthat::skip_on_cran()
  testthat::skip_if_offline(host = "ec.europa.eu")
  suppressPackageStartupMessages(library(inflationinequality))

  threshold <- 0.3
  countries <- c("DE", "FR")

  results <- data.table::rbindlist(lapply(countries, function(country) {
    inflation <- calculate_inflation(
      country,
      "income",
      level = 2,
      start_year = 2019,
      end_year = 2026,
      end_month = 4,
      weighting_method = "ras"
    )

    comparison <- compare_to_official_hicp(inflation)
    summary <- data.table::as.data.table(comparison$summary)
    summary[, country := country]
    summary
  }))

  expect_true(
    all(results$max_abs_difference <= threshold),
    info = paste(
      results[, paste0(country, "=", round(max_abs_difference, 4))],
      collapse = "; "
    )
  )
})
