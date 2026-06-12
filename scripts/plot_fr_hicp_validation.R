devtools::load_all(".")

inflation <- calculate_inflation(
  "FR",
  "income",
  level = 2,
  start_year = 2019,
  end_year = 2026,
  end_month = 4,
  weighting_method = "ras"
)

comparison <- compare_to_official_hicp(inflation)
comparison_plot <- comparison$plot +
  ggplot2::labs(title = NULL, subtitle = NULL, x = NULL, y = NULL, caption = NULL) +
  ggplot2::theme(
    plot.title = ggplot2::element_blank(),
    plot.subtitle = ggplot2::element_blank(),
    plot.caption = ggplot2::element_blank()
  )

out <- "docs/france_hicp_validation_mean_vs_published.png"
ggplot2::ggsave(out, comparison_plot, width = 9, height = 5.5, dpi = 160)
vignette_out <- "vignettes/figures/france_hicp_validation_mean_vs_published.png"
ggplot2::ggsave(vignette_out, comparison_plot, width = 9, height = 5.5, dpi = 160)

print(comparison$summary)
cat("saved:", out, "\n")
cat("saved:", vignette_out, "\n")

indices <- calculate_price_indices(
  "FR",
  "income",
  level = 2,
  start_year = 2010,
  end_year = 2026,
  end_month = 4,
  base_year = 2010,
  weighting_method = "ras"
)

level_comparison <- compare_to_official_hicp(indices)
level_comparison_plot <- level_comparison$plot +
  ggplot2::labs(title = NULL, subtitle = NULL, x = NULL, y = NULL, caption = NULL) +
  ggplot2::theme(
    plot.title = ggplot2::element_blank(),
    plot.subtitle = ggplot2::element_blank(),
    plot.caption = ggplot2::element_blank()
  )

level_out <- "docs/france_hicp_validation_level_mean_vs_published.png"
ggplot2::ggsave(level_out, level_comparison_plot, width = 9, height = 5.5, dpi = 160)
vignette_level_out <- "vignettes/figures/france_hicp_validation_level_mean_vs_published.png"
ggplot2::ggsave(vignette_level_out, level_comparison_plot, width = 9, height = 5.5, dpi = 160)

print(level_comparison$summary)
cat("saved:", level_out, "\n")
cat("saved:", vignette_level_out, "\n")
