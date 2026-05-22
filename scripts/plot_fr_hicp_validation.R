devtools::load_all(".")

inflation <- calculate_inflation(
  "FR",
  "income",
  start_year = 2019,
  end_year = 2026,
  end_month = 3
)

comparison <- compare_to_official_hicp(inflation)

out <- "docs/france_hicp_validation_mean_vs_published.png"
ggplot2::ggsave(out, comparison$plot, width = 9, height = 5.5, dpi = 160)

print(comparison$summary)
cat("saved:", out, "\n")

indices <- calculate_price_indices(
  "FR",
  "income",
  start_year = 2010,
  end_year = 2026,
  end_month = 3,
  base_year = 2010
)

level_comparison <- compare_to_official_hicp(indices)

level_out <- "docs/france_hicp_validation_level_mean_vs_published.png"
ggplot2::ggsave(level_out, level_comparison$plot, width = 9, height = 5.5, dpi = 160)

print(level_comparison$summary)
cat("saved:", level_out, "\n")
