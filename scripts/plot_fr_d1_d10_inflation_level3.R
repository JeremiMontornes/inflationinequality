devtools::load_all(".")

indices <- calculate_price_indices(
  "FR",
  "income",
  level = 3,
  start_year = 2010,
  end_year = 2026,
  end_month = 3,
  base_year = 2010,
  recode_ecoicop2_to_ecoicop1 = TRUE
)

plot_dt <- data.table::copy(indices$dt)
plot_dt <- plot_dt[grepl("cile1$|cile10$", category)]
plot_dt[, series := data.table::fifelse(grepl("cile1$", category), "D1", "D10")]

out <- "docs/france_hicp_inflation_d1_d10_level3_insee_recode_2010_2026_03.png"

p <- ggplot2::ggplot(plot_dt, ggplot2::aes(x = date, y = annual_rate, color = series)) +
  ggplot2::geom_hline(yintercept = 0, color = "grey55", linewidth = 0.3) +
  ggplot2::geom_line(linewidth = 0.9, na.rm = TRUE) +
  ggplot2::scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  ggplot2::scale_color_manual(values = c("D1" = "#2f6fbb", "D10" = "#c84a31")) +
  ggplot2::labs(
    x = "",
    y = "Glissement annuel (%)",
    color = "",
    title = "Inflation IPCH par catégorie de ménages",
    subtitle = "France, D1 et D10, niveau 3 INSEE, recodage ECOICOP v2 vers v1"
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")
  )

ggplot2::ggsave(out, p, width = 9, height = 5.5, dpi = 160)

summary_dt <- plot_dt[
  !is.na(annual_rate),
  .(
    first_date = min(date),
    last_date = max(date),
    mean_inflation = mean(annual_rate),
    max_inflation = max(annual_rate),
    min_inflation = min(annual_rate),
    last_inflation = annual_rate[date == max(date)]
  ),
  by = series
][order(series)]

print(summary_dt)
cat("saved:", out, "\n")
