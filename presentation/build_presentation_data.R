library(data.table)
library(ggplot2)

pkgload::load_all(".", quiet = TRUE)

out_dir <- file.path("presentation", "output")
fig_dir <- file.path(out_dir, "figures")
data_dir <- file.path(out_dir, "data")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

start_year <- 2021
end_year <- 2026
end_month <- 3
x_axis_end <- as.Date("2026-03-01")
x_axis_breaks <- c(as.Date(sprintf("%s-01-01", start_year:(end_year - 1))), x_axis_end)
x_axis_labels <- c(as.character(start_year:(end_year - 1)), "mars 2026")

cache_dir <- file.path(data_dir, "cache")
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

cached <- function(path, expr) {
  if (file.exists(path)) {
    return(readRDS(path))
  }
  value <- force(expr)
  saveRDS(value, path)
  value
}

highest_fr_level <- function(category) {
  if (identical(category, "income")) 3 else 2
}

fr_cpi_cached <- function(level) {
  cached(
    file.path(cache_dir, sprintf("fr_cpi_level%s_2019_2026m03.rds", level)),
    load_cpi("FR", level = level, start_year = start_year - 2, end_year = end_year, end_month = end_month)
  )
}

fr_weights_cached <- function(level) {
  cached(
    file.path(cache_dir, sprintf("fr_index_weights_level%s_2019_2026.rds", level)),
    load_index_weights("FR", level = level, start_year = start_year - 2, end_year = end_year)
  )
}

fr_hbs_path <- function(category) {
  file.path(cache_dir, sprintf("fr_hbs_%s_level2_all.rds", category))
}

load_fr_hbs_cached <- function(category) {
  cached(
    fr_hbs_path(category),
    load_hbs("FR", category, level = 2)
  )
}

calc_fr <- function(category) {
  level <- highest_fr_level(category)
  args <- list(
    country = "FR",
    category = category,
    level = level,
    start_year = start_year,
    custom_cpi = fr_cpi_cached(level),
    custom_index_weights = fr_weights_cached(level),
    interpolated_hbs = FALSE
  )
  if (level == 2) {
    args$custom_hbs <- load_fr_hbs_cached(category)
    args$specific_hbs_year <- 2020
  } else if (identical(category, "income")) {
    args$france_insee_income_groups <- "quintile"
  }
  do.call(calculate_inflation, args)
}

calc_fr_contributions <- function(category) {
  level <- highest_fr_level(category)
  args <- list(
    country = "FR",
    category = category,
    level = level,
    start_year = start_year,
    custom_cpi = fr_cpi_cached(level),
    custom_index_weights = fr_weights_cached(level),
    interpolated_hbs = FALSE
  )
  if (level == 2) {
    args$custom_hbs <- load_fr_hbs_cached(category)
    args$specific_hbs_year <- 2020
  } else if (identical(category, "income")) {
    args$france_insee_income_groups <- "quintile"
  }
  do.call(calculate_contributions, args)
}

lt_cache <- file.path(data_dir, "data_LT_income_inflation_2021_2026m03.rds")
calc_lt_income <- function() {
  if (file.exists(lt_cache)) {
    return(readRDS(lt_cache))
  }
  x <- calculate_inflation(
    "LT", "income",
    level = 2,
    start_year = start_year,
    end_year = end_year,
    interpolated_hbs = TRUE
  )
  saveRDS(x, lt_cache)
  x
}

calc_country_income_level <- function(country, label, file_prefix) {
  cpi_obj <- cached(
    file.path(cache_dir, sprintf("%s_cpi_level2_2020_2026m03.rds", tolower(country))),
    load_cpi(country, level = 2, start_year = 2020, end_year = 2026, end_month = 3)
  )
  weights_obj <- cached(
    file.path(cache_dir, sprintf("%s_index_weights_level2_2020_2026.rds", tolower(country))),
    load_index_weights(country, level = 2, start_year = 2020, end_year = 2026)
  )
  hbs_obj <- cached(
    file.path(cache_dir, sprintf("%s_hbs_income_level2_all.rds", tolower(country))),
    load_hbs(country, "income", level = 2)
  )
  inflation <- calculate_inflation(
    country, "income",
    level = 2,
    start_year = 2022,
    custom_cpi = cpi_obj,
    custom_index_weights = weights_obj,
    custom_hbs = hbs_obj,
    interpolated_hbs = FALSE,
    specific_hbs_year = 2020
  )
  plot_level(
    inflation,
    "Inflation par niveau de vie",
    sprintf("%s, 1er quintile, 5e quintile et moyenne, donnees mensuelles 2022-mars 2026", label),
    sprintf("Sources : package inflationinequality, HICP Eurostat et HBS %s.", country),
    sprintf("fig_%s_income_inflation_level_2022_2026m03.png", file_prefix),
    c("1er quintile" = "#1B998B", "5e quintile" = "#D95D39", "Moyenne" = "#111827"),
    axis_start_year = 2022
  )
}

in_requested_window <- function(dt) {
  dt[(year > start_year | (year == start_year & month >= 1)) &
       (year < end_year | (year == end_year & month <= end_month))]
}

format_date <- function(dt) {
  dt[, date := as.Date(sprintf("%04d-%02d-01", year, month))]
  dt
}

gap_from_inflation <- function(inflation, label, low_label, high_label) {
  gap <- calculate_inflation_gap(inflation)
  format_date(gap)
  gap <- in_requested_window(gap)
  gap[, `:=`(
    dimension = label,
    low_label = low_label,
    high_label = high_label
  )]
  gap[]
}

fr_income <- calc_fr("income")
fr_age <- calc_fr("age")
fr_urban <- calc_fr("urban")
fr_urban_contrib <- calc_fr_contributions("urban")
lt_income <- calc_lt_income()

gaps <- rbindlist(list(
  gap_from_inflation(fr_income, "France - revenu", "1er quintile", "5e quintile"),
  gap_from_inflation(fr_age, "France - age", "moins de 30 ans", "60 ans ou plus"),
  gap_from_inflation(fr_urban, "France - residence", "rural", "villes"),
  gap_from_inflation(lt_income, "Lituanie - revenu", "1er quintile", "5e quintile")
), fill = TRUE)

fwrite(gaps, file.path(data_dir, "data_FR_demographic_inflation_gaps_2021_2026m03.csv"))

plot_gap <- function(dt, title, subtitle, caption, file, color) {
  max_row <- dt[which.max(abs(inflation_gap))]
  ggplot(dt, aes(date, inflation_gap)) +
    geom_hline(yintercept = 0, color = "#9CA3AF", linewidth = 0.6) +
    geom_line(color = color, linewidth = 1.35, lineend = "round") +
    geom_point(
      data = max_row,
      aes(date, inflation_gap),
      color = color,
      fill = "white",
      shape = 21,
      stroke = 1.2,
      size = 3.4
    ) +
    annotate(
      "label",
      x = max_row$date,
      y = max_row$inflation_gap,
      label = sprintf("%+.1f pt", max_row$inflation_gap),
      color = "#111827",
      fill = "white",
      family = "sans",
      fontface = "bold",
      size = 4.2,
      vjust = ifelse(max_row$inflation_gap >= 0, -0.85, 1.85)
    ) +
    scale_x_date(
      breaks = x_axis_breaks,
      labels = x_axis_labels,
      limits = c(as.Date(sprintf("%s-01-01", start_year)), x_axis_end),
      expand = expansion(mult = c(0.01, 0.04))
    ) +
    scale_y_continuous(
      labels = function(x) sprintf("%+.1f", x),
      expand = expansion(mult = c(0.16, 0.18))
    ) +
    labs(
      title = title,
      subtitle = subtitle,
      x = NULL,
      y = "Ecart d'inflation, en points",
      caption = caption
    ) +
    theme_minimal(base_size = 16) +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "#E5E7EB", linewidth = 0.5),
      plot.title = element_text(face = "bold", color = "#111827", size = 22),
      plot.subtitle = element_text(color = "#4B5563", margin = margin(b = 14), size = 14),
      axis.text = element_text(color = "#374151"),
      axis.title.y = element_text(color = "#374151", margin = margin(r = 8)),
      plot.caption = element_text(color = "#6B7280", hjust = 0, size = 10, margin = margin(t = 12))
    )
  ggsave(file.path(fig_dir, file), width = 9.6, height = 5.4, dpi = 220, bg = "white")
}

plot_gap(
  gaps[dimension == "France - revenu"],
  "Les ecarts d'inflation varient avec le niveau de vie",
  "France, 1er quintile moins 5e quintile, donnees mensuelles 2021-mars 2026",
  "Sources : package inflationinequality, HICP Eurostat et HBS INSEE niveau 3.",
  "fig_FR_income_inflation_gap_level3_2021_2026m03.png",
  "#1B998B"
)

plot_level <- function(inflation, title, subtitle, caption, file, colors, axis_start_year = start_year) {
  lowest_category <- inflation$categories[1]
  highest_category <- inflation$categories[length(inflation$categories)]
  dt <- copy(in_requested_window(inflation$dt))
  total <- dt[, .(inflation = mean(inflation), category = "Moyenne"), by = .(year, month)]
  dt <- rbindlist(list(dt, total), use.names = TRUE)
  dt <- dt[category %in% c(lowest_category, highest_category, "Moyenne")]
  dt[, date := as.Date(sprintf("%04d-%02d-01", year, month))]
  dt[, category := fifelse(category == lowest_category, names(colors)[1],
    fifelse(category == highest_category, names(colors)[2], "Moyenne")
  )]
  dt[, category := factor(category, levels = names(colors))]

  axis_breaks <- c(as.Date(sprintf("%s-01-01", axis_start_year:(end_year - 1))), x_axis_end)
  axis_labels <- c(as.character(axis_start_year:(end_year - 1)), "mars 2026")

  ggplot(dt, aes(date, inflation, color = category)) +
    geom_line(linewidth = 1.15, lineend = "round") +
    scale_color_manual(values = colors) +
    scale_x_date(
      breaks = axis_breaks,
      labels = axis_labels,
      limits = c(as.Date(sprintf("%s-01-01", axis_start_year)), x_axis_end),
      expand = expansion(mult = c(0.01, 0.04))
    ) +
    scale_y_continuous(
      labels = function(x) sprintf("%.1f", x),
      expand = expansion(mult = c(0.14, 0.16))
    ) +
    labs(
      title = title,
      subtitle = subtitle,
      x = NULL,
      y = "Inflation, en %",
      color = NULL,
      caption = caption
    ) +
    theme_minimal(base_size = 16) +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "#E5E7EB", linewidth = 0.5),
      plot.title = element_text(face = "bold", color = "#111827", size = 22),
      plot.subtitle = element_text(color = "#4B5563", margin = margin(b = 14), size = 14),
      axis.text = element_text(color = "#374151"),
      axis.title.y = element_text(color = "#374151", margin = margin(r = 8)),
      legend.position = "bottom",
      legend.text = element_text(size = 11, color = "#374151"),
      plot.caption = element_text(color = "#6B7280", hjust = 0, size = 10, margin = margin(t = 10))
    )
  ggsave(file.path(fig_dir, file), width = 9.6, height = 5.4, dpi = 220, bg = "white")
}

plot_level(
  fr_income,
  "Inflation par niveau de vie",
  "France, 1er quintile, 5e quintile et moyenne, donnees mensuelles 2021-mars 2026",
  "Sources : package inflationinequality, HICP Eurostat et HBS INSEE niveau 3.",
  "fig_FR_income_inflation_level3_2021_2026m03.png",
  c("1er quintile" = "#1B998B", "5e quintile" = "#D95D39", "Moyenne" = "#111827")
)

calc_country_income_level("LV", "Lettonie", "LV")

plot_level(
  fr_age,
  "Inflation par tranche d'age",
  "France, moins de 30 ans, 60 ans ou plus et moyenne, donnees mensuelles 2021-mars 2026",
  "Sources : package inflationinequality, IPC et Budget de famille / HBS.",
  "fig_FR_age_inflation_level_2021_2026m03.png",
  c("Moins de 30 ans" = "#D95D39", "60 ans ou plus" = "#3B5BA5", "Moyenne" = "#111827")
)

plot_level(
  fr_urban,
  "Inflation par zone de residence",
  "France, rural, villes et moyenne, donnees mensuelles 2021-mars 2026",
  "Sources : package inflationinequality, IPC et Budget de famille / HBS.",
  "fig_FR_urban_inflation_level_2021_2026m03.png",
  c("Rural" = "#3B5BA5", "Villes" = "#1B998B", "Moyenne" = "#111827")
)

plot_gap(
  gaps[dimension == "France - age"],
  "L'age change le panier de consommation expose aux prix",
  "France, moins de 30 ans moins 60 ans ou plus, donnees mensuelles 2021-mars 2026",
  "Sources : package inflationinequality, IPC et Budget de famille / HBS.",
  "fig_FR_age_inflation_gap_2021_2026m03.png",
  "#D95D39"
)

plot_gap(
  gaps[dimension == "France - residence"],
  "Le lieu de residence module l'inflation subie",
  "France, rural moins villes, donnees mensuelles 2021-mars 2026",
  "Sources : package inflationinequality, IPC et Budget de famille / HBS.",
  "fig_FR_urban_inflation_gap_2021_2026m03.png",
  "#3B5BA5"
)

plot_urban_contribution_gap <- function(contributions) {
  low <- contributions$categories[1]
  high <- contributions$categories[length(contributions$categories)]
  dt <- copy(in_requested_window(contributions$dt)[category %in% c(low, high)])
  dt[, poste := fifelse(substr(coicop, 1, 2) == "01", "Alimentation",
    fifelse(substr(coicop, 1, 3) %in% c("044", "045"), "Energie",
      fifelse(substr(coicop, 1, 2) == "07", "Transport",
        fifelse(substr(coicop, 1, 2) %in% c("04", "05"), "Logement hors energie", "Autres postes")
      )
    )
  )]
  dt_poste <- dt[, .(contribution = sum(contribution)), by = .(year, month, category, poste)]
  wide <- dcast(dt_poste, year + month + poste ~ category, value.var = "contribution")
  wide[, contribution_gap := get(low) - get(high)]
  wide[, date := as.Date(sprintf("%04d-%02d-01", year, month))]
  total <- wide[, .(total_gap = sum(contribution_gap)), by = .(date, year, month)]

  poste_order <- c("Alimentation", "Energie", "Transport", "Logement hors energie", "Autres postes")
  wide[, poste := factor(poste, levels = poste_order)]

  ggplot(wide, aes(date, contribution_gap, fill = poste)) +
    geom_hline(yintercept = 0, color = "#9CA3AF", linewidth = 0.6) +
    geom_col(width = 24, alpha = 0.92) +
    geom_line(data = total, aes(date, total_gap, group = 1), inherit.aes = FALSE,
      color = "#111827", linewidth = 1.15) +
    scale_fill_manual(values = c(
      "Alimentation" = "#1B998B",
      "Energie" = "#F2A541",
      "Transport" = "#3B5BA5",
      "Logement hors energie" = "#D95D39",
      "Autres postes" = "#9CA3AF"
    )) +
    scale_x_date(
      breaks = x_axis_breaks,
      labels = x_axis_labels,
      limits = c(as.Date(sprintf("%s-01-01", start_year)), x_axis_end),
      expand = expansion(mult = c(0.01, 0.04))
    ) +
    scale_y_continuous(
      labels = function(x) sprintf("%+.1f", x),
      expand = expansion(mult = c(0.16, 0.18))
    ) +
    labs(
      title = "Ce qui explique l'ecart d'inflation rural-villes",
      subtitle = "France, contribution des postes de consommation a l'ecart mensuel 2021-mars 2026",
      x = NULL,
      y = "Contribution a l'ecart, en points",
      fill = NULL,
      caption = "Sources : package inflationinequality, IPC et Budget de famille / HBS."
    ) +
    theme_minimal(base_size = 16) +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "#E5E7EB", linewidth = 0.5),
      plot.title = element_text(face = "bold", color = "#111827", size = 22),
      plot.subtitle = element_text(color = "#4B5563", margin = margin(b = 14), size = 14),
      axis.text = element_text(color = "#374151"),
      axis.title.y = element_text(color = "#374151", margin = margin(r = 8)),
      legend.position = "bottom",
      legend.text = element_text(size = 10, color = "#374151"),
      plot.caption = element_text(color = "#6B7280", hjust = 0, size = 10, margin = margin(t = 10))
    )
  ggsave(file.path(fig_dir, "fig_FR_urban_contribution_gap_2021_2026m03.png"), width = 9.6, height = 5.4, dpi = 220, bg = "white")
}

plot_urban_contribution_gap(fr_urban_contrib)

plot_gap(
  gaps[dimension == "Lituanie - revenu"],
  "Le meme outil se transpose a la Lituanie",
  "Lituanie, 1er quintile moins 5e quintile, donnees mensuelles 2021-mars 2026",
  "Sources : package inflationinequality, Eurostat HICP et HBS.",
  "fig_LT_income_inflation_gap_2021_2026m03.png",
  "#7C3AED"
)

message("Presentation data and figures written to ", normalizePath(out_dir, winslash = "/"))
