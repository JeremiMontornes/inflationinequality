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
  country = c("France", "Belgium", "Italy"),
  coicop_case = c(
    paste(
      "France income level = 3 uses bundled INSEE Budget de famille 2017",
      "data at 4-digit COICOP. Eurostat HBS income data remain the default",
      "for harmonised level = 2 comparisons."
    ),
    paste(
      "No Belgium-specific override is implemented yet; Eurostat harmonised",
      "HBS income categories are used if available. Belgium is the strongest",
      "candidate for a country-specific quartile rule."
    ),
    paste(
      "If the Eurostat 2010 income HBS wave is available, Italy uses that",
      "wave for all CPI weight years; other Eurostat HBS waves are not used",
      "by default."
    )
  ),
  population_group_caveat = c(
    paste(
      "INSEE published income-decile IPC series are by 'niveau de vie':",
      "household disposable income divided by consumption units. This is not",
      "strictly the same grouping concept as the Eurostat HBS income-quintile",
      "tables used by load_hbs()."
    ),
    paste(
      "Bruegel treats Belgium with bottom/top quartiles rather than quintiles.",
      "The package has no automatic Belgium quartile override yet."
    ),
    paste(
      "Bruegel warns that I.Stat data used in earlier versions identify",
      "expenditure quintiles, not income quintiles; Italy was therefore",
      "removed from their final figures."
    )
  ),
  housing_caveat = c(
    paste(
      "Large housing-weight gaps can partly reflect the different income",
      "grouping concepts. In HICP calculations, CP042 imputed rents are not",
      "directly used because they are absent from the HICP basket; observed",
      "differences mainly affect CP041 actual rents and related housing items."
    ),
    "",
    ""
  ),
  bruegel_comparison = c(
    paste(
      "Bruegel uses country-specific national sources where they support",
      "finer COICOP detail; France is the implemented national-source level",
      "3 case in this package."
    ),
    paste(
      "Belgium is treated with bottom/top quartiles and national HBS waves;",
      "2018 weights are used for 2019-2020 and 2020 weights for 2021-2022."
    ),
    paste(
      "Bruegel warns that I.Stat data used in earlier versions are expenditure",
      "quintiles, not income quintiles, and reports removing Italy from the",
      "final figures."
    )
  ),
  package_status = c(
    paste(
      "Implemented for level = 3 income calculations with",
      "france_insee_income_groups = 'decile' or 'quintile'."
    ),
    "Under development; use custom_hbs for national quartile workflows.",
    paste(
      "Partly implemented through the fixed 2010 Eurostat income HBS default;",
      "national-source level = 3 support is not integrated."
    )
  ),
  stringsAsFactors = FALSE
)

save(methodology_metadata, file = "data/methodology_metadata.rda")
save(country_coicop_special_cases,
     file = "data/country_coicop_special_cases.rda")
