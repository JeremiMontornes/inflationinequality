pkgload::load_all(".")

library(data.table)
library(ggplot2)

start_year <- 2020L
end_year <- 2026L
end_month <- 4L
level <- 2L
hicp_level <- 3L
aggregate_geo <- "EA20"
selected_categories <- c("First quintile", "Fifth quintile")

out_dir <- file.path(getwd(), "docs")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

country_weights <- load_country_weights(
  aggregate_geo,
  start_year = start_year,
  end_year = end_year
)
countries <- unique(country_weights$country)

make_country_components <- function(country) {
  message("Building pooled components for ", country)
  country_i <- country

  index_weights_obj <- load_index_weights(
    country,
    level = hicp_level,
    start_year = start_year,
    end_year = end_year
  )
  index_weights_obj <- recode_index_weights_ecoicop2_to_ecoicop1(
    index_weights_obj,
    target_level = level
  )

  hbs_obj <- load_hbs(country, "income", level = level)
  specific_hbs_year <- NULL
  if (identical(country, "IT") && 2010 %in% hbs_obj$dt[, unique(year)]) {
    specific_hbs_year <- 2010
  }

  components <- merge_index_and_hbs(
    index_weights(
      data.table::copy(index_weights_obj$dt),
      country = index_weights_obj$country,
      level = index_weights_obj$level,
      base_total = index_weights_obj$base_total
    ),
    hbs_obj,
    specific_hbs_year = specific_hbs_year
  )

  components <- components[
    ,
    {
      temp <- .SD
      if (.N > 0 && any(year <= weight_year)) {
        temp <- temp[year <= weight_year]
        temp[, .SD[which.max(year)]]
      } else {
        temp[, .SD[which.min(year)]]
      }
    },
    by = .(coicop, category, weight_year)
  ]

  components <- components[category %in% selected_categories]
  components[
    ,
    hbs_multiplier := data.table::fifelse(
      consumption == 1e-6 & total_consumption == 1e-6,
      1e-6,
      consumption / total_consumption
    )
  ]
  components[, component := weight * hbs_multiplier]

  country_weight_dt <- country_weights[
    country == country_i,
    .(weight_year = year, country_weight)
  ]
  components <- merge(
    components,
    country_weight_dt,
    by = "weight_year",
    all.x = TRUE
  )
  components[, pooled_component := component * country_weight]
  components[
    ,
    .(pooled_component = sum(pooled_component, na.rm = TRUE)),
    by = .(coicop, category, weight_year)
  ]
}

pooled_components <- data.table::rbindlist(
  lapply(countries, make_country_components),
  use.names = TRUE
)

pooled_weights <- pooled_components[
  ,
  .(pooled_component = sum(pooled_component, na.rm = TRUE)),
  by = .(coicop, category, weight_year)
]
pooled_weights[
  ,
  weighted_consumption := pooled_component * 100 / sum(pooled_component),
  by = .(category, weight_year)
]
pooled_weights <- pooled_weights[
  ,
  .(coicop, category, year = weight_year, weighted_consumption)
]

cpi_raw <- download_hicp_dataset(
  id = "prc_hicp_minr",
  filters = list(freq = "M", geo = aggregate_geo, unit = "I15"),
  date.range = c(sprintf("%s-01", start_year - 1L), sprintf("%s-%02d", end_year, end_month))
)
coicop_col <- if ("coicop18" %in% names(cpi_raw)) "coicop18" else "coicop"
cpi_dt <- cpi_raw[
  grepl("^CP\\d+$", get(coicop_col)),
  .(
    series_name = paste0("prc_hicp_minr.", unit, ".", get(coicop_col), ".", geo),
    coicop = get(coicop_col),
    value = values,
    year = as.integer(substr(time, 1, 4)),
    month = as.integer(substr(time, 6, 7))
  )
]
cpi_dt <- select_coicop_level(cpi_dt, hicp_level)
cpi_basket <- unique(cpi_dt[, .(year, month)])
cpi_basket[
  ,
  `:=`(
    series_name = "EA20 all-items placeholder",
    value = 1
  )
]
data.table::setcolorder(cpi_basket, c("series_name", "value", "year", "month"))
cpi_obj <- cpi(
  dt = cpi_dt,
  dt_basket = cpi_basket,
  country = aggregate_geo,
  level = hicp_level
)
cpi_obj <- recode_cpi_ecoicop2_to_ecoicop1(cpi_obj, target_level = level)

price_dt <- data.table::copy(cpi_obj$dt)
price_dt[, date := as.Date(sprintf("%04d-%02d-01", year, month))]
data.table::setorder(price_dt, coicop, date)
price_dt[, dec_ratio := hicp::unchain(x = value, t = date), by = coicop]

hicp_data <- merge(
  price_dt,
  pooled_weights,
  by = c("coicop", "year"),
  allow.cartesian = TRUE
)

index_dt <- hicp_data[
  !is.na(dec_ratio) & !is.na(weighted_consumption),
  .(laspeyres = hicp::laspeyres(x = dec_ratio, w0 = weighted_consumption)),
  by = .(category, year, month, date)
]
data.table::setorder(index_dt, category, date)
index_dt[, chain_laspeyres := hicp::chain(x = laspeyres, t = date, by = 12), by = category]
index_dt[
  ,
  price_index := rebase_or_first_available(
    chain_laspeyres,
    t = date,
    t.ref = as.character(start_year)
  ),
  by = category
]
index_dt <- index_dt[
  (year > start_year | (year == start_year & month >= 1L)) &
    (year < end_year | (year == end_year & month <= end_month))
]

plot_dt <- index_dt[
  category %in% selected_categories,
  .(
    date,
    price_index,
    series = data.table::fifelse(category == "First quintile", "Q1 pooled HBS", "Q5 pooled HBS")
  )
]

official_raw <- download_hicp_dataset(
  id = "prc_hicp_minr",
  filters = list(freq = "M", geo = aggregate_geo, unit = "I15", coicop18 = "TOTAL"),
  date.range = c(sprintf("%s-01", start_year), sprintf("%s-%02d", end_year, end_month))
)
official <- as.data.table(official_raw)
official[, date := as.Date(paste0(time, "-01"))]
official <- official[
  ,
  .(
    date,
    price_index = hicp::rebase(values, t = date, t.ref = as.character(start_year)),
    series = "Official HICP"
  )
]

plot_dt <- data.table::rbindlist(list(plot_dt, official), use.names = TRUE)

p <- ggplot(plot_dt, aes(x = date, y = price_index, color = series, linetype = series)) +
  geom_line(linewidth = 1) +
  scale_color_manual(
    values = c(
      "Official HICP" = "#4c4c4c",
      "Q1 pooled HBS" = "#1b9e77",
      "Q5 pooled HBS" = "#d95f02"
    ),
    name = NULL
  ) +
  scale_linetype_manual(
    values = c("Official HICP" = "dashed", "Q1 pooled HBS" = "solid", "Q5 pooled HBS" = "solid"),
    name = NULL
  ) +
  labs(
    title = "EA20 price indices with pooled HBS weights",
    subtitle = "Q1, Q5 and official all-items HICP, level 2 COICOP, base 2020 = 100",
    x = NULL,
    y = "Price index"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

out <- file.path(out_dir, "ea20_q1_q5_pooled_hbs_official_hicp_price_indices_2020_2026_04.png")
ggsave(out, p, width = 8.4, height = 4.9, dpi = 180)

cat("saved=", out, "\n", sep = "")
cat("rows=", nrow(plot_dt), " latest=", max(plot_dt$date), "\n", sep = "")
print(plot_dt[order(date), .SD[c(1, .N)], by = series, .SDcols = c("date", "price_index")])
