pkgload::load_all(".")

library(data.table)
library(ggplot2)

start_year <- 2000L
base_year <- 2000L
end_year <- 2026L
end_month <- 4L

indices <- calculate_price_indices(
  "EA20",
  "income",
  start_year = start_year,
  end_year = end_year,
  end_month = end_month,
  base_year = base_year,
  include_total = TRUE
)

plot_dt <- data.table::copy(indices$dt)[
  category %in% c("First quintile", "Fifth quintile", "Total")
]
plot_dt[
  ,
  series := data.table::fcase(
    category == "First quintile", "Q1",
    category == "Fifth quintile", "Q5",
    category == "Total", "Recalculated total"
  )
]

subtitle <- sprintf(
  "Income quintiles, EA20 country aggregation, level 2 COICOP, base %s = 100",
  indices$base_year
)

p <- ggplot(plot_dt, aes(x = date, y = price_index, color = series, linetype = series)) +
  geom_line(linewidth = 0.9, na.rm = TRUE) +
  scale_color_manual(
    values = c(
      "Recalculated total" = "#4c4c4c",
      "Q1" = "#1b9e77",
      "Q5" = "#d95f02"
    ),
    breaks = c("Recalculated total", "Q1", "Q5"),
    name = NULL
  ) +
  scale_linetype_manual(
    values = c("Recalculated total" = "dashed", "Q1" = "solid", "Q5" = "solid"),
    breaks = c("Recalculated total", "Q1", "Q5"),
    name = NULL
  ) +
  scale_x_date(date_breaks = "4 years", date_labels = "%Y") +
  labs(
    title = "EA20 price indices by income quintile",
    subtitle = subtitle,
    x = NULL,
    y = "Price index"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

out_dir <- file.path("docs")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
out <- file.path(
  out_dir,
  sprintf("ea20_q1_q5_price_indices_%s_%s_%02d_base%s.png",
          min(plot_dt$year), end_year, end_month, indices$base_year)
)
ggsave(out, p, width = 9.2, height = 5.4, dpi = 180)

csv_out <- sub("\\.png$", ".csv", out)
data.table::fwrite(
  plot_dt[, .(series, category, year, month, date, price_index, annual_rate)],
  csv_out
)

cat("saved=", normalizePath(out, winslash = "/"), "\n", sep = "")
cat("csv=", normalizePath(csv_out, winslash = "/"), "\n", sep = "")
cat("start=", min(plot_dt$date), " end=", max(plot_dt$date), " base_year=", indices$base_year, "\n", sep = "")
print(plot_dt[order(date), .SD[c(1, .N)], by = series, .SDcols = c("date", "price_index")])
