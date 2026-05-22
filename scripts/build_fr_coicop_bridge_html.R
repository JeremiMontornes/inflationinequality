devtools::load_all(".")

bridge <- build_coicop_bridge(
  country = "FR",
  category = "income",
  level = 2,
  start_year = 2010,
  end_year = 2026,
  recode_ecoicop2_to_ecoicop1 = TRUE
)

out <- "docs/france_coicop_hicp_hbs_bridge_income_level2_2010_2026.html"
write_coicop_bridge_html(
  bridge,
  out,
  title = "France income COICOP HICP-HBS bridge, level 2, 2010-2026",
  compact = TRUE,
  latest_weight_year_only = TRUE,
  include_category = FALSE
)

audit_out <- "docs/france_coicop_hicp_hbs_bridge_income_level2_2010_2026_audit.html"
write_coicop_bridge_html(
  bridge,
  audit_out,
  title = "France income COICOP HICP-HBS bridge audit, level 2, 2010-2026",
  compact = FALSE,
  latest_weight_year_only = TRUE,
  include_category = FALSE
)

latest_bridge <- bridge[weight_year == max(weight_year)]
print(unique(latest_bridge[, .(hicp_coicop, hbs_coicop, mapping_status)])[, .N, by = mapping_status])
cat("saved audit:", audit_out, "\n")
cat("saved:", out, "\n")
