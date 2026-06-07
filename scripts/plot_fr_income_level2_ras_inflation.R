pkgload::load_all(".", quiet = TRUE)

dir.create("docs/validation", showWarnings = FALSE, recursive = TRUE)

inflation_obj <- calculate_inflation(
  "FR", "income",
  level = 2,
  start_year = 2020,
  weighting_method = "ras"
)

dt <- data.table::copy(inflation_obj$dt)
dt[, date := as.Date(sprintf("%04d-%02d-01", year, month))]

total_dt <- dt[
  ,
  .(inflation = mean(inflation), category = "Total"),
  by = .(year, month, date)
]

plot_dt <- data.table::rbindlist(
  list(dt[category %in% inflation_obj$categories], total_dt),
  use.names = TRUE
)
label_map <- data.table::data.table(
  category = c(inflation_obj$categories, "Total"),
  series = c("Q1", "Q2", "Q3", "Q4", "Q5", "Total")
)
plot_dt <- label_map[plot_dt, on = "category"]

p <- ggplot2::ggplot(plot_dt, ggplot2::aes(x = date, y = inflation, color = series)) +
  ggplot2::geom_line(linewidth = 0.8) +
  ggplot2::scale_color_manual(
    values = c(
      "Q1" = "#1f77b4",
      "Q2" = "#17becf",
      "Q3" = "#2ca02c",
      "Q4" = "#ff7f0e",
      "Q5" = "#d62728",
      "Total" = "#222222"
    ),
    breaks = c("Q1", "Q2", "Q3", "Q4", "Q5", "Total")
  ) +
  ggplot2::scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  ggplot2::labs(
    title = "France: inflation by income quintile, RAS weights",
    subtitle = 'calculate_inflation("FR", "income", level = 2, weighting_method = "ras")',
    x = NULL,
    y = "Inflation (%)",
    color = NULL,
    caption = "Source: inflationinequality, Eurostat HICP weights and HBS. RAS calibration on income quintiles."
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    legend.position = "top",
    plot.title = ggplot2::element_text(face = "bold"),
    panel.grid.minor = ggplot2::element_blank()
  )

out <- normalizePath(
  "docs/validation/calculate_inflation_FR_income_level2_ras_all_quintiles.png",
  mustWork = FALSE
)
ggplot2::ggsave(out, p, width = 9, height = 5.5, dpi = 160, bg = "white")

print(plot_dt[date == as.Date("2026-04-01"), .(series, inflation)][order(series)])
cat("saved:", out, "\n")
