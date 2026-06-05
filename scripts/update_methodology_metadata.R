load("data/methodology_metadata.rda")

methodology_metadata <- methodology_metadata[
  !methodology_metadata$topic %in% c(
    "Price data source",
    "HBS source",
    "Imputed rents",
    "Official HICP comparison",
    "Belgium (BE)",
    "Italy (IT)"
  ),
]

methodology_metadata[
  methodology_metadata$topic == "COICOP level",
  "package_default"
] <- paste(
  "COICOP 3 digits by default (package level = 2);",
  "COICOP 2 to 4 digits are accepted by load and calculation functions."
)

methodology_metadata[
  methodology_metadata$topic == "COICOP level",
  "notes"
] <- paste(
  "Eurostat HBS income data are generally available at 3-digit COICOP",
  "(package level = 2), which is the package default."
)

methodology_metadata[
  methodology_metadata$topic == "HBS-to-CPI timing",
  "package_default"
] <- paste(
  "HBS waves are not available for all countries in the same years.",
  "For each CPI weight year, the package uses the closest available HBS",
  "wave at or before that year; if no earlier wave exists, it uses the",
  "earliest available HBS wave."
)

methodology_metadata[
  methodology_metadata$topic == "HBS-to-CPI timing",
  "notes"
] <- paste(
  "This temporal merge is country-specific because Eurostat HBS waves differ",
  "across countries. Users can force a wave with specific_hbs_year, interpolate",
  "waves with interpolated_hbs = TRUE, or provide custom_hbs/custom_index_weights."
)

country_coicop_special_cases <- data.frame(
  country = c("France", "Italy", "Spain", "Euro area"),
  coicop_case = c(
    paste(
      "Level = 3 uses bundled INSEE Budget de famille 2017 national HBS",
      "at 4-digit COICOP for income, age, and residence-area groups."
    ),
    paste(
      "Income level = 2 uses reconstructed income-quintile baskets from",
      "Istat microdata and Eurostat all-households totals."
    ),
    paste(
      "Level = 3 uses compact INE EPF 2020 microdata-derived HBS objects",
      "for income, age, and residence-area groups."
    ),
    paste(
      "Euro-area aggregates are built from country-level HICP-HBS",
      "calculations and Eurostat HICP country weights."
    )
  ),
  population_group_caveat = c(
    paste(
      "National groups may differ from harmonised Eurostat groups;",
      "income can be used as deciles or adjacent-decile quintiles."
    ),
    paste(
      "Italy income groups are estimated ventilations, not directly",
      "disseminated Eurostat income-quintile HBS tables."
    ),
    paste(
      "Spain groups use equivalised income, reference-person age, and",
      "density of residence from EPF microdata."
    ),
    paste(
      "Household groups are defined within each country; euro-area results",
      "are not a pooled household distribution."
    )
  ),
  bruegel_comparison = c(
    "National-source level-3 workflow implemented.",
    "Estimated input; users can replace it with custom_hbs.",
    "National microdata input; users can replace it with custom_hbs.",
    ""
  ),
  package_status = c(
    "Implemented for level = 3 income, age, and urban calculations.",
    "Implemented for level = 2 income calculations.",
    "Implemented for level = 3 income, age, and urban calculations.",
    "Implemented through country-level calculations; unavailable inputs are excluded."
  ),
  stringsAsFactors = FALSE
)

save(methodology_metadata, file = "data/methodology_metadata.rda")
save(country_coicop_special_cases,
     file = "data/country_coicop_special_cases.rda")
