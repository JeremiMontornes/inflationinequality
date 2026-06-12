pkgload::load_all(".")

library(data.table)
library(ggplot2)
library(xml2)

out_dir <- "man/figures"

idbank_to_decile <- c(
  "010054292" = "D1",
  "010054293" = "D2",
  "010054299" = "D8",
  "010054300" = "D9"
)

insee_url <- paste0(
  "https://bdm.insee.fr/series/sdmx/data/SERIES_BDM/",
  paste(names(idbank_to_decile), collapse = "+"),
  "?startPeriod=2010&endPeriod=2023"
)

doc <- read_xml(insee_url)
series_nodes <- xml_find_all(doc, ".//*[local-name()='Series']")

official <- rbindlist(lapply(series_nodes, function(series_node) {
  idbank <- xml_attr(series_node, "IDBANK")
  obs_nodes <- xml_find_all(series_node, "./*[local-name()='Obs']")

  data.table(
    idbank = idbank,
    decile = unname(idbank_to_decile[idbank]),
    year = as.integer(xml_attr(obs_nodes, "TIME_PERIOD")),
    official_index = as.numeric(xml_attr(obs_nodes, "OBS_VALUE")),
    official_title = xml_attr(series_node, "TITLE_FR")
  )
}))

official <- official[!is.na(decile)]

cpi_level3 <- readRDS("vignettes/articles/INSEE_CPI_level3.RDS")
index_weights_level3 <- readRDS("vignettes/articles/INSEE_CPI_index_weights_level3.RDS")
hbs_level3 <- readRDS("vignettes/articles/INSEE_HBS_2017_level3.RDS")

hbs_level3[["start_year"]] <- 2010
hbs_level3[["end_year"]] <- 2023

price_indices <- calculate_price_indices(
  "FR",
  "income",
  level = 3,
  start_year = 2010,
  end_year = 2023,
  custom_cpi = cpi_level3,
  custom_index_weights = index_weights_level3,
  custom_hbs = hbs_level3,
  base_year = 2015,
  include_total = FALSE
)

category_map <- data.table(
  category = hbs_level3$categories[c(1, 2, 8, 9)],
  decile = c("D1", "D2", "D8", "D9")
)

calculated <- merge(price_indices$dt, category_map, by = "category")[
  ,
  .(calculated_index = mean(price_index, na.rm = TRUE)),
  by = .(decile, year)
]

comparison <- merge(official, calculated, by = c("decile", "year"), all.x = TRUE)
comparison[, diff_index_points := calculated_index - official_index]
setorder(comparison, decile, year)

csv_path <- file.path(
  out_dir,
  "france-insee-decile-official-vs-recalculated-index-level-D1-D2-D8-D9-2010-2023.csv"
)
fwrite(comparison, csv_path)

comparison_long <- melt(
  comparison,
  id.vars = c("decile", "year"),
  measure.vars = c("official_index", "calculated_index"),
  variable.name = "series",
  value.name = "index"
)

comparison_long[
  ,
  series := fifelse(
    series == "official_index",
    "INSEE official",
    "inflationinequality recalculated"
  )
]

comparison_plot <- ggplot(
  comparison_long,
  aes(year, index, color = series, linetype = series)
) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.4) +
  facet_wrap(~decile, ncol = 2) +
  scale_color_manual(values = c(
    "INSEE official" = "#111111",
    "inflationinequality recalculated" = "#D55E00"
  )) +
  scale_linetype_manual(values = c(
    "INSEE official" = "solid",
    "inflationinequality recalculated" = "22"
  )) +
  scale_x_continuous(breaks = seq(2010, 2023, 2)) +
  labs(
    title = NULL,
    subtitle = NULL,
    x = NULL,
    y = NULL,
    color = NULL,
    linetype = NULL,
    caption = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    plot.caption = element_blank(),
    strip.text = element_text(face = "bold")
  )

comparison_png <- file.path(
  out_dir,
  "france-insee-decile-official-vs-recalculated-index-level-D1-D2-D8-D9-2010-2023.png"
)
ggsave(comparison_png, comparison_plot, width = 10, height = 6, dpi = 160)

gap_plot <- ggplot(comparison, aes(year, diff_index_points, color = decile)) +
  geom_hline(yintercept = 0, color = "grey50") +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.5) +
  scale_x_continuous(breaks = seq(2010, 2023, 2)) +
  labs(
    title = NULL,
    subtitle = NULL,
    x = NULL,
    y = NULL,
    color = NULL,
    caption = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    plot.caption = element_blank()
  )

gap_png <- file.path(
  out_dir,
  "france-insee-decile-index-level-gap-D1-D2-D8-D9-2010-2023.png"
)
ggsave(gap_png, gap_plot, width = 9, height = 5.2, dpi = 160)

print(comparison[
  ,
  .(
    min_year = min(year),
    max_year = max(year),
    mean_abs_gap = round(mean(abs(diff_index_points), na.rm = TRUE), 3),
    max_abs_gap = round(max(abs(diff_index_points), na.rm = TRUE), 3)
  ),
  by = decile
])

cat("Saved:\n")
cat("-", csv_path, "\n")
cat("-", comparison_png, "\n")
cat("-", gap_png, "\n")
