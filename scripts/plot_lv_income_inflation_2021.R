library(inflationinequality)
library(ggplot2)

inflation_lv <- calculate_inflation("LV", "income", start_year = 2021)

p <- plot_time_series(inflation_lv) +
  labs(
    title = "Latvia inflation by income quintile",
    subtitle = "Year-on-year HICP inflation, income quintiles, since 2021",
    x = NULL,
    y = "Inflation (%)",
    caption = "Source: Eurostat HICP and HBS, package inflationinequality."
  ) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  )

out <- file.path("docs", "lv_income_inflation_2021.png")
dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
ggsave(out, p, width = 9, height = 5.2, dpi = 300, bg = "white")

message("Exported: ", normalizePath(out, winslash = "/"))
