suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(inflationinequality)
})

years_requested <- c(2005L, 2015L, 2020L)
out_dir <- file.path("docs", "italy_hbs_income_quintile_distribution")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

hbs_it <- load_hbs(
  country = "IT",
  category = "income",
  level = 2,
  start_year = min(years_requested),
  end_year = max(years_requested)
)

dt <- copy(hbs_it$dt)
dt <- dt[year %in% years_requested]

if (nrow(dt) == 0L) {
  stop("No Italy HBS income-quintile observations found for requested years.")
}

available_years <- sort(unique(dt$year))
missing_years <- setdiff(years_requested, available_years)
if (length(missing_years) > 0L) {
  warning(
    "Requested year(s) not available in Eurostat HBS income-quintile data: ",
    paste(missing_years, collapse = ", ")
  )
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

dist <- dt[, .(
  consumption = sum(consumption, na.rm = TRUE)
), by = .(year, category, coicop_div = substr(coicop, 1L, 2L))]

dist[, total_consumption := sum(consumption, na.rm = TRUE), by = .(year, category)]
dist[, share := 100 * consumption / total_consumption]
dist[, coicop_label := division_labels[coicop_div]]
dist[is.na(coicop_label), coicop_label := coicop_div]

category_order <- hbs_it$categories
dist[, category := factor(category, levels = category_order)]
dist[, coicop_label := factor(coicop_label, levels = rev(unname(division_labels)))]

csv_path <- file.path(out_dir, "it_income_quintile_consumption_distribution_2005_2015_2020.csv")
png_path <- file.path(out_dir, "it_income_quintile_consumption_distribution_2005_2015_2020.png")

fwrite(dist[order(year, category, coicop_div)], csv_path)

p <- ggplot(
  dist,
  aes(x = category, y = share, fill = coicop_label)
) +
  geom_col(width = 0.75) +
  facet_wrap(~ year, nrow = 1) +
  scale_fill_brewer(palette = "Paired", direction = -1) +
  labs(
    x = NULL,
    y = "Share of total expenditure (%)",
    fill = NULL,
    title = "Italy consumption distribution by income quintile",
    subtitle = "HBS consumption baskets by income quintile and COICOP division"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    legend.key.width = unit(0.8, "cm"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 30, hjust = 1)
  )

ggsave(png_path, p, width = 12, height = 7, dpi = 180)

summary_gap <- dist[
  category %in% c(category_order[1L], category_order[length(category_order)]),
  .(share = sum(share)),
  by = .(year, category, coicop_div, coicop_label)
]

fwrite(
  summary_gap[order(year, coicop_div, category)],
  file.path(out_dir, "it_q1_q5_consumption_distribution_summary.csv")
)

message("Available years: ", paste(available_years, collapse = ", "))
if (length(missing_years) > 0L) {
  message("Missing requested years: ", paste(missing_years, collapse = ", "))
}
message("Wrote: ", normalizePath(csv_path, winslash = "/", mustWork = FALSE))
message("Wrote: ", normalizePath(png_path, winslash = "/", mustWork = FALSE))
