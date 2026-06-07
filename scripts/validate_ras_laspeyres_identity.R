pkgload::load_all(".", quiet = TRUE)

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

dt <- data.table::copy(indices_obj$dt)

quintile_mean <- dt[
  category != "Total",
  .(
    mean_q_laspeyres = mean(laspeyres),
    mean_q_chain = mean(chain_laspeyres),
    mean_q_index = mean(price_index),
    mean_q_rate = mean(annual_rate, na.rm = TRUE)
  ),
  by = .(year, month, date)
]

total <- dt[
  category == "Total",
  .(
    year,
    month,
    date,
    total_laspeyres = laspeyres,
    total_chain = chain_laspeyres,
    total_index = price_index,
    total_rate = annual_rate
  )
]

comparison <- merge(quintile_mean, total, by = c("year", "month", "date"))
comparison[, diff_laspeyres := mean_q_laspeyres - total_laspeyres]
comparison[, diff_chain := mean_q_chain - total_chain]
comparison[, diff_index := mean_q_index - total_index]
comparison[, diff_rate := mean_q_rate - total_rate]

cat("\nMean Q1-Q5 vs Total inside calculate_price_indices():\n")
print(comparison[
  ,
  .(
    max_abs_laspeyres = max(abs(diff_laspeyres), na.rm = TRUE),
    max_abs_chain = max(abs(diff_chain), na.rm = TRUE),
    max_abs_index = max(abs(diff_index), na.rm = TRUE),
    max_abs_rate = max(abs(diff_rate), na.rm = TRUE)
  )
])

cat("\nApril 2026:\n")
print(comparison[
  year == 2026 & month == 4,
  .(date, diff_laspeyres, diff_chain, diff_index, diff_rate)
])
