library(data.table)
library(ggplot2)

options(rdbnomics.use_readLines = TRUE)
devtools::load_all(".")

cache_file <- file.path(tempdir(), "ea20_hicp_validation_indices_2015_2026_04.rds")
if (file.exists(cache_file)) {
  indices <- readRDS(cache_file)
} else {
  indices <- calculate_price_indices(
    "EA20",
    "income",
    start_year = 2015,
    end_year = 2026,
    end_month = 4,
    base_year = 2025,
    include_total = TRUE
  )
  dir.create(dirname(cache_file), showWarnings = FALSE, recursive = TRUE)
  saveRDS(indices, cache_file)
}

calculated <- copy(indices$dt)[
  category == "Total",
  .(year, month, date, calculated_value = price_index)
]

official_cpi <- load_cpi(
  "EA20",
  level = 2,
  start_year = 2015,
  end_year = 2026,
  end_month = 4
)

official <- copy(official_cpi$dt_basket)
official[, date := as.Date(sprintf("%04d-%02d-01", year, month))]
setorder(official, date)
official[, official_value := rebase_or_first_available(
  x = value,
  t = date,
  t.ref = "2025"
)]
official <- official[, .(year, month, date, official_value)]

comparison <- merge(
  calculated,
  official,
  by = c("year", "month", "date"),
  all.x = TRUE
)
comparison[, difference := calculated_value - official_value]

summary_dt <- comparison[
  !is.na(calculated_value) & !is.na(official_value),
  .(
    n = .N,
    mean_difference = mean(difference),
    mean_abs_difference = mean(abs(difference)),
    rmse = sqrt(mean(difference^2)),
    max_abs_difference = max(abs(difference))
  )
]

line_dt <- melt(
  comparison,
  id.vars = c("year", "month", "date", "difference"),
  measure.vars = c("calculated_value", "official_value"),
  variable.name = "series",
  value.name = "value"
)
line_dt[, series := fifelse(
  series == "calculated_value",
  "Recalculated EA20 total",
  "Official EA20 HICP"
)]

p <- ggplot(comparison, aes(x = date)) +
  geom_col(
    aes(y = difference),
    fill = "grey78",
    color = "grey78",
    width = 25,
    na.rm = TRUE
  ) +
  geom_hline(yintercept = 0, color = "grey35", linewidth = 0.3) +
  geom_line(
    data = line_dt,
    aes(y = value, color = series),
    linewidth = 0.9,
    na.rm = TRUE
  ) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  scale_color_manual(
    values = c(
      "Recalculated EA20 total" = "#111111",
      "Official EA20 HICP" = "#2f6fbb"
    )
  ) +
  labs(
    x = NULL,
    y = "Index level and difference, 2025 = 100",
    color = NULL,
    title = "EA20 recalculated total price index vs official HICP",
    subtitle = "Level 2 COICOP, chained unchained movements, country aggregation with HICP country weights",
    caption = "Grey bars: recalculated minus official HICP, in index points."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

out_dir <- file.path("docs")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
ggsave(
  filename = file.path(out_dir, "ea20_hicp_validation_total_vs_official_2015_2026_04.png"),
  plot = p,
  width = 9,
  height = 5.4,
  dpi = 160
)

fwrite(
  comparison,
  file.path(out_dir, "ea20_hicp_validation_total_vs_official_2015_2026_04.csv")
)
fwrite(
  summary_dt,
  file.path(out_dir, "ea20_hicp_validation_total_vs_official_2015_2026_04_summary.csv")
)

print(summary_dt)
