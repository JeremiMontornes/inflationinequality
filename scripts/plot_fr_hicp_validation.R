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

out <- "docs/france_hicp_validation_mean_vs_published.png"
ggplot2::ggsave(out, comparison$plot, width = 9, height = 5.5, dpi = 160)
vignette_out <- "vignettes/figures/france_hicp_validation_mean_vs_published.png"
ggplot2::ggsave(vignette_out, comparison$plot, width = 9, height = 5.5, dpi = 160)

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
  base_year = 2015,
  weighting_method = "ras"
)

level_comparison <- compare_to_official_hicp(indices)

level_out <- "docs/france_hicp_validation_level_mean_vs_published.png"
ggplot2::ggsave(level_out, level_comparison$plot, width = 9, height = 5.5, dpi = 160)
vignette_level_out <- "vignettes/figures/france_hicp_validation_level_mean_vs_published.png"
ggplot2::ggsave(vignette_level_out, level_comparison$plot, width = 9, height = 5.5, dpi = 160)

print(level_comparison$summary)
cat("saved:", level_out, "\n")
cat("saved:", vignette_level_out, "\n")
