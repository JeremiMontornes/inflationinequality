pkgload::load_all(".", quiet = TRUE)

dir.create("docs/validation", showWarnings = FALSE, recursive = TRUE)

indices_obj <- calculate_price_indices(
  "FR", "income",
  level = 2,
  start_year = 2019,
  base_year = 2015,
  weighting_method = "ras",
  include_total = TRUE
)

plot_dt <- data.table::copy(indices_obj$dt)
label_map <- data.table::data.table(
  category = c(indices_obj$categories[indices_obj$categories != "Total"], "Total"),
  series = c("Q1", "Q2", "Q3", "Q4", "Q5", "Total")
)
plot_dt <- label_map[plot_dt, on = "category"]

p <- ggplot2::ggplot(plot_dt, ggplot2::aes(x = date, y = price_index, color = series)) +
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
    title = "France: price indices by income quintile, RAS weights",
    subtitle = 'calculate_price_indices("FR", "income", level = 2, start_year = 2019, base_year = 2015, weighting_method = "ras")',
    x = NULL,
    y = paste0("Price index, base ", indices_obj$base_year, " = 100"),
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
  "docs/validation/calculate_price_indices_FR_income_level2_ras_from_2019.png",
  mustWork = FALSE
)
ggplot2::ggsave(out, p, width = 9, height = 5.5, dpi = 160, bg = "white")

latest_date <- max(plot_dt$date, na.rm = TRUE)
print(plot_dt[date == latest_date, .(series, price_index, annual_rate)][order(series)])
cat("latest_date:", as.character(latest_date), "\n")
cat("saved:", out, "\n")
