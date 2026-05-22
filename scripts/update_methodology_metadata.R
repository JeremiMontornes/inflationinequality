load("data/methodology_metadata.rda")

methodology_metadata <- methodology_metadata[
  !methodology_metadata$topic %in% c(
    "Price data source",
    "HBS source",
    "Imputed rents",
    "Official HICP comparison"
  ),
]

methodology_metadata[
  methodology_metadata$topic == "COICOP level",
  "package_default"
] <- "COICOP digits 2 by default; digits 2 to 4 are accepted by load and calculation functions."

save(methodology_metadata, file = "data/methodology_metadata.rda")
