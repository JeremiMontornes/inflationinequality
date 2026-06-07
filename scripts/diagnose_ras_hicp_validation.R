pkgload::load_all(".", quiet = TRUE)

compare_rate <- function(calculated_dt, official_dt) {
  out <- merge(
    calculated_dt[, .(year, month, date, calculated_value)],
    official_dt[, .(year, month, date, official_value)],
    by = c("year", "month", "date"),
    all.x = TRUE
  )
  out[, difference := calculated_value - official_value]
  out[
    !is.na(calculated_value) & !is.na(official_value),
    .(
      n = .N,
      mean_difference = mean(difference),
      mean_abs_difference = mean(abs(difference)),
      rmse = sqrt(mean(difference^2)),
      max_abs_difference = max(abs(difference))
    )
  ]
}

make_official_rate <- function(cpi_obj) {
  dt <- data.table::copy(cpi_obj$dt_basket)
  dt[, date := as.Date(sprintf("%04d-%02d-01", year, month))]
  data.table::setorder(dt, date)
  dt[, official_value := hicp::rates(value, t = date, type = "year")]
  dt[, .(year, month, date, official_value)]
}

inflation_obj <- calculate_inflation(
  "FR", "income",
  level = 2,
  start_year = 2019,
  end_year = 2026,
  end_month = 4,
  weighting_method = "ras"
)

calculated_inflation <- calculate_total_inflation(inflation_obj)
calculated_inflation[, date := as.Date(sprintf("%04d-%02d-01", year, month))]
data.table::setnames(calculated_inflation, "total_inflation", "calculated_value")

official_v2 <- load_cpi(
  "FR",
  level = 2,
  start_year = 2018,
  end_year = 2026,
  end_month = 4
)

cpi_v1 <- load_cpi(
  "FR",
  level = 3,
  start_year = 2018,
  end_year = 2026,
  end_month = 4
)
cpi_v1 <- recode_cpi_ecoicop2_to_ecoicop1(cpi_v1, target_level = 2)

cat("\nAgainst published all-items HICP basket loaded at level 2:\n")
print(compare_rate(calculated_inflation, make_official_rate(official_v2)))

cat("\nAgainst reconstructed ECOICOP v1 level-2 basket from item indices:\n")
print(compare_rate(calculated_inflation, make_official_rate(cpi_v1)))

cat("\nLargest differences against reconstructed ECOICOP v1 basket:\n")
comparison_v1 <- merge(
  calculated_inflation[, .(year, month, date, calculated_value)],
  make_official_rate(cpi_v1),
  by = c("year", "month", "date"),
  all.x = TRUE
)
comparison_v1[, difference := calculated_value - official_value]
print(comparison_v1[order(-abs(difference))][1:12])

indices_obj <- calculate_price_indices(
  "FR", "income",
  level = 2,
  start_year = 2019,
  end_year = 2026,
  end_month = 4,
  base_year = 2015,
  weighting_method = "ras",
  include_total = TRUE
)

official_total <- make_official_rate(official_v2)
indices_total <- data.table::copy(indices_obj$dt)[
  category == "Total",
  .(year, month, date, calculated_value = annual_rate)
]
quintile_mean_rate <- data.table::copy(indices_obj$dt)[
  category != "Total",
  .(calculated_value = mean(annual_rate, na.rm = TRUE)),
  by = .(year, month, date)
]

cat("\nAnnual rate of calculate_price_indices Total vs published HICP:\n")
print(compare_rate(indices_total, official_total))

cat("\nMean of Q1-Q5 annual rates from chained indices vs published HICP:\n")
print(compare_rate(quintile_mean_rate, official_total))

cat("\nApril 2026 rates:\n")
print(merge(
  data.table::rbindlist(list(
    calculated_inflation[year == 2026 & month == 4, .(series = "calculate_inflation mean", value = calculated_value)],
    indices_total[year == 2026 & month == 4, .(series = "price_indices Total", value = calculated_value)],
    quintile_mean_rate[year == 2026 & month == 4, .(series = "mean Q1-Q5 annual_rate", value = calculated_value)]
  )),
  official_total[year == 2026 & month == 4, .(official_value)],
  by = NULL
))
