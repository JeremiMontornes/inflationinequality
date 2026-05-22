devtools::load_all(".")

cpi_obj <- load_cpi("FR", level = 3, end_year = 2026, end_month = 3)
hbs_obj <- readRDS("tests/testthat/fixtures/hbs_fr_income2.RDS")

indices <- calculate_price_indices(
  country = "FR",
  category = "income",
  level = 2,
  end_year = 2026,
  end_month = 3,
  custom_cpi = cpi_obj,
  custom_hbs = hbs_obj,
  base_year = 2010,
  include_total = FALSE,
  recode_ecoicop2_to_ecoicop1 = TRUE
)

q_dt <- data.table::copy(indices$dt)[
  category %in% c("First quintile", "Fifth quintile"),
  .(date, series = data.table::fifelse(category == "First quintile", "Q1", "Q5"), index = price_index)
]

published <- data.table::copy(cpi_obj$dt_basket)
published[, date := as.Date(sprintf("%04d-%02d-01", year, month))]
published[, index := hicp::rebase(value, t = date, t.ref = "2010")]
published <- published[, .(date, series = "IPCH publie", index)]

plot_dt <- data.table::rbindlist(list(published, q_dt), use.names = TRUE)
plot_dt <- plot_dt[date <= as.Date("2026-03-01") & !is.na(index)]

p <- ggplot2::ggplot(plot_dt, ggplot2::aes(x = date, y = index, color = series)) +
  ggplot2::geom_line(linewidth = 0.8) +
  ggplot2::scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  ggplot2::scale_y_continuous(
    breaks = seq(
      floor(min(plot_dt$index, na.rm = TRUE) / 10) * 10,
      ceiling(max(plot_dt$index, na.rm = TRUE) / 10) * 10,
      by = 10
    )
  ) +
  ggplot2::scale_color_manual(
    values = c("IPCH publie" = "#222222", "Q1" = "#2f6fbb", "Q5" = "#c84a31")
  ) +
  ggplot2::labs(
    title = "France: IPCH publie et indices par quintile de revenu",
    subtitle = "Indice en niveau, base 2010 = 100, jusqu'a mars 2026",
    x = NULL,
    y = "Indice",
    color = NULL,
    caption = "Source: Eurostat and national statistical institutes, HICP-HBS."
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    legend.position = "top",
    plot.title = ggplot2::element_text(face = "bold"),
    panel.grid.minor = ggplot2::element_blank()
  )

out <- "docs/france_hicp_published_index_level_q1_q5_ecoicop1_base2010_to_2026_03.png"
ggplot2::ggsave(out, p, width = 9, height = 5.5, dpi = 160)

latest <- plot_dt[date == max(date), .(series, index)]
print(latest)
cat("saved:", out, "\n")
