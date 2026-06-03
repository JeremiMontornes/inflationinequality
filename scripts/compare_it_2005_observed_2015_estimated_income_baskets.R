suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(inflationinequality)
})

out_dir <- file.path("docs", "italy_hbs_income_quintile_distribution")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

hbs_it <- load_hbs(
  country = "IT",
  category = "income",
  level = 2,
  start_year = 2005,
  end_year = 2015
)

dt_2005 <- copy(hbs_it$dt[year == 2005])
total_2005 <- copy(hbs_it$dt_total[year == 2005])
total_2015 <- copy(hbs_it$dt_total[year == 2015])

if (nrow(dt_2005) == 0L) {
  stop("Observed Italy 2005 income-quintile HBS data are missing.")
}
if (nrow(total_2015) == 0L) {
  stop("Italy 2015 total HBS consumption data are missing.")
}
if (nrow(total_2005) == 0L) {
  total_2005 <- dt_2005[, .(
    total_consumption = mean(consumption, na.rm = TRUE)
  ), by = coicop]
  total_2005[, year := 2005L]
}

division_labels <- c(
  "01" = "Food and non-alcoholic beverages",
  "02" = "Alcoholic beverages and tobacco",
  "03" = "Clothing and footwear",
  "04" = "Housing, water, electricity, gas",
  "05" = "Furnishings and household equipment",
  "06" = "Health",
  "07" = "Transport",
  "08" = "Communication",
  "09" = "Recreation and culture",
  "10" = "Education",
  "11" = "Restaurants and hotels",
  "12" = "Miscellaneous goods and services"
)

# Estimate 2015 income-quintile baskets by preserving the 2005 relative
# income-quintile intensity within each COICOP item and applying it to the
# 2015 all-households consumption total. This is not an observed 2015 income
# quintile: it is a projection anchored on 2005 cross-quintile differences.
correction_2005 <- merge(
  dt_2005[, .(coicop, category, consumption_2005 = consumption)],
  total_2005[, .(coicop, total_2005 = total_consumption)],
  by = "coicop",
  all.x = TRUE
)
correction_2005[, hbs_intensity_2005 := consumption_2005 / total_2005]

estimated_2015 <- merge(
  correction_2005[, .(coicop, category, hbs_intensity_2005)],
  total_2015[, .(coicop, total_2015 = total_consumption)],
  by = "coicop",
  all.x = TRUE
)
estimated_2015[, consumption := hbs_intensity_2005 * total_2015]
estimated_2015[, year := 2015L]
estimated_2015[, source := "2015 estimated from 2005 income intensities"]

observed_2005 <- dt_2005[, .(coicop, category, consumption)]
observed_2005[, year := 2005L]
observed_2005[, source := "2005 observed"]

combined <- rbindlist(
  list(
    observed_2005,
    estimated_2015[, .(coicop, category, consumption, year, source)]
  ),
  use.names = TRUE
)

dist <- combined[, .(
  consumption = sum(consumption, na.rm = TRUE)
), by = .(year, source, category, coicop_div = substr(coicop, 1L, 2L))]
dist[, total_consumption := sum(consumption, na.rm = TRUE), by = .(year, source, category)]
dist[, share := 100 * consumption / total_consumption]
dist[, coicop_label := division_labels[coicop_div]]
dist[is.na(coicop_label), coicop_label := coicop_div]

category_order <- hbs_it$categories
dist[, category := factor(category, levels = category_order)]
dist[, coicop_label := factor(coicop_label, levels = rev(unname(division_labels)))]
dist[, panel := factor(
  source,
  levels = c("2005 observed", "2015 estimated from 2005 income intensities")
)]

csv_path <- file.path(out_dir, "it_2005_observed_2015_estimated_income_baskets.csv")
png_path <- file.path(out_dir, "it_2005_observed_2015_estimated_income_baskets.png")
fwrite(dist[order(year, category, coicop_div)], csv_path)

p <- ggplot(dist, aes(x = category, y = share, fill = coicop_label)) +
  geom_col(width = 0.75) +
  facet_wrap(~ panel, nrow = 1) +
  scale_fill_brewer(palette = "Paired", direction = -1) +
  labs(
    x = NULL,
    y = "Share of total expenditure (%)",
    fill = NULL,
    title = "Italy consumption baskets by income quintile",
    subtitle = "2005 observed vs 2015 estimated from 2005 income-quintile intensities"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    legend.key.width = unit(0.8, "cm"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 30, hjust = 1)
  )

ggsave(png_path, p, width = 12, height = 7, dpi = 180)

gap <- dcast(
  dist[category %in% c(category_order[1L], category_order[length(category_order)])],
  year + source + coicop_div + coicop_label ~ category,
  value.var = "share"
)
if (all(c(category_order[1L], category_order[length(category_order)]) %in% names(gap))) {
  setnames(gap, c(category_order[1L], category_order[length(category_order)]), c("q1_share", "q5_share"))
  gap[, q1_minus_q5 := q1_share - q5_share]
  fwrite(
    gap[order(year, coicop_div)],
    file.path(out_dir, "it_2005_observed_2015_estimated_q1_q5_gaps.csv")
  )
}

message("Wrote: ", normalizePath(csv_path, winslash = "/", mustWork = FALSE))
message("Wrote: ", normalizePath(png_path, winslash = "/", mustWork = FALSE))
