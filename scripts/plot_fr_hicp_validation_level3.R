devtools::load_all(".")

indices <- calculate_price_indices(
  "FR",
  "income",
  level = 3,
  start_year = 2010,
  end_year = 2026,
  end_month = 3,
  base_year = 2010
)

level_comparison <- compare_to_official_hicp(indices)

out <- "docs/france_hicp_validation_level3_insee_mean_vs_published.png"
ggplot2::ggsave(out, level_comparison$plot, width = 9, height = 5.5, dpi = 160)

print(level_comparison$summary)
cat("saved:", out, "\n")
