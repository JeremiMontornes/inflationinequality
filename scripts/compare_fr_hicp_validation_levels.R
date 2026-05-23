devtools::load_all(".")

compare_level <- function(level) {
  indices <- calculate_price_indices(
    "FR",
    "income",
    level = level,
    start_year = 2010,
    end_year = 2026,
    end_month = 3,
    base_year = 2010
  )
  comparison <- compare_to_official_hicp(indices)
  summary <- data.table::copy(comparison$summary)
  summary[, level := level]
  summary[, first_date := min(comparison$dt$date)]
  summary[, last_date := max(comparison$dt$date)]
  summary[, last_difference := comparison$dt[date == max(date), difference]]
  summary[]
}

out <- data.table::rbindlist(
  list(compare_level(2), compare_level(3)),
  use.names = TRUE
)

data.table::setcolorder(
  out,
  c("level", "n", "first_date", "last_date", "mean_difference",
    "mean_abs_difference", "rmse", "max_abs_difference", "last_difference")
)

print(out)
