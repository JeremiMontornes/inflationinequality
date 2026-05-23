devtools::load_all(".")

build_inflation_comparison <- function(level) {
  indices <- calculate_price_indices(
    "FR",
    "income",
    level = level,
    start_year = 2010,
    end_year = 2026,
    end_month = 3,
    base_year = 2010,
    recode_ecoicop2_to_ecoicop1 = TRUE
  )

  calculated_dt <- data.table::copy(indices$dt)
  calculated_dt <- calculated_dt[category != "Total"]
  calculated_dt <- calculated_dt[
    ,
    .(calculated_inflation = mean(annual_rate, na.rm = TRUE)),
    by = .(year, month, date)
  ]

  official_cpi <- load_cpi(
    "FR",
    level = level,
    start_year = 2010,
    end_year = 2026,
    end_month = 3
  )
  official_dt <- data.table::copy(official_cpi$dt_basket)
  official_dt[, date := as.Date(sprintf("%04d-%02d-01", year, month))]
  data.table::setorder(official_dt, date)
  official_dt[, official_inflation := hicp::rates(value, t = date, type = "year")]
  official_dt <- official_dt[, .(year, month, date, official_inflation)]

  comparison_dt <- merge(
    calculated_dt,
    official_dt,
    by = c("year", "month", "date"),
    all.x = TRUE
  )
  comparison_dt[, difference := calculated_inflation - official_inflation]
  comparison_dt <- comparison_dt[!is.na(calculated_inflation) & !is.na(official_inflation)]

  summary_dt <- comparison_dt[
    ,
    .(
      n = .N,
      mean_difference = mean(difference),
      mean_abs_difference = mean(abs(difference)),
      rmse = sqrt(mean(difference^2)),
      max_abs_difference = max(abs(difference))
    )
  ]
  summary_dt[, level := level]

  line_dt <- data.table::melt(
    comparison_dt,
    id.vars = c("year", "month", "date", "difference"),
    measure.vars = c("calculated_inflation", "official_inflation"),
    variable.name = "series",
    value.name = "value"
  )
  line_dt[, series := data.table::fifelse(
    series == "calculated_inflation",
    "Inflation moyenne calculee",
    "IPCH publie"
  )]

  plot <- ggplot2::ggplot(comparison_dt, ggplot2::aes(x = date)) +
    ggplot2::geom_col(
      ggplot2::aes(y = difference),
      fill = "grey78",
      color = "grey78",
      width = 25,
      na.rm = TRUE
    ) +
    ggplot2::geom_hline(yintercept = 0, color = "grey40", linewidth = 0.3) +
    ggplot2::geom_line(
      data = line_dt,
      ggplot2::aes(y = value, color = series),
      linewidth = 0.85,
      na.rm = TRUE
    ) +
    ggplot2::scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
    ggplot2::scale_color_manual(
      values = c("Inflation moyenne calculee" = "black", "IPCH publie" = "#2f6fbb")
    ) +
    ggplot2::labs(
      x = "",
      y = "Glissement annuel / ecart (points de pourcentage)",
      color = "",
      title = paste0("Inflation moyenne calculee vs IPCH publie - niveau ", level),
      subtitle = "France, groupes de revenu, recode_ecoicop2_to_ecoicop1 = TRUE"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title = ggplot2::element_text(face = "bold")
    )

  list(dt = comparison_dt, summary = summary_dt, plot = plot)
}

comparison_level2 <- build_inflation_comparison(2)
comparison_level3 <- build_inflation_comparison(3)

out_level2 <- "docs/france_hicp_inflation_validation_level2_recode_mean_vs_published.png"
out_level3 <- "docs/france_hicp_inflation_validation_level3_insee_recode_mean_vs_published.png"

ggplot2::ggsave(out_level2, comparison_level2$plot, width = 9, height = 5.5, dpi = 160)
ggplot2::ggsave(out_level3, comparison_level3$plot, width = 9, height = 5.5, dpi = 160)

summary_dt <- data.table::rbindlist(
  list(comparison_level2$summary, comparison_level3$summary),
  use.names = TRUE
)
data.table::setcolorder(
  summary_dt,
  c("level", "n", "mean_difference", "mean_abs_difference", "rmse", "max_abs_difference")
)

print(summary_dt)
cat("saved:", out_level2, "\n")
cat("saved:", out_level3, "\n")
