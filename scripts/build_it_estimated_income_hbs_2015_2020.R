suppressPackageStartupMessages({
  library(data.table)
  library(inflationinequality)
})

out_dir <- file.path("data-raw", "italy_estimated_hbs")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

level <- 2L
observed_income_year <- 2005L
target_years <- c(2015L, 2020L)

hbs_it <- load_hbs(
  country = "IT",
  category = "income",
  level = level,
  start_year = observed_income_year,
  end_year = max(target_years)
)

dt_obs <- copy(hbs_it$dt[year == observed_income_year])
if (nrow(dt_obs) == 0L) {
  stop("Italy observed income-quintile HBS data are missing for ", observed_income_year)
}

target_totals <- copy(hbs_it$dt_total[year %in% target_years])
if (nrow(target_totals) == 0L) {
  stop("Italy total HBS data are missing for target years: ",
       paste(target_years, collapse = ", "))
}

# Eurostat returns Italy income-quintile HBS for 2005 but not an all-households
# total for 2005 in the same table. Since income quintiles are equal-population
# groups, use the simple mean across quintiles as the reference total for the
# 2005 relative-intensity calculation.
total_obs <- dt_obs[, .(
  total_observed = mean(consumption, na.rm = TRUE)
), by = coicop]

intensity <- merge(
  dt_obs[, .(coicop, category, observed_consumption = consumption)],
  total_obs,
  by = "coicop",
  all.x = TRUE
)
intensity <- intensity[is.finite(total_observed) & total_observed > 0]
intensity[, hbs_intensity := observed_consumption / total_observed]

project_one_year <- function(year_i) {
  totals_i <- target_totals[
    year == year_i &
      is.finite(total_consumption) &
      total_consumption > 0,
    .(coicop, total_consumption)
  ]

  projected <- merge(
    intensity[, .(coicop, category, hbs_intensity)],
    totals_i,
    by = "coicop",
    all = FALSE
  )
  projected[, consumption := hbs_intensity * total_consumption]
  projected <- projected[is.finite(consumption) & consumption > 0]
  projected[, `:=`(
    series_name = sprintf(
      "Italy HBS %s estimated from %s income-quintile intensities",
      year_i, observed_income_year
    ),
    year = as.numeric(year_i)
  )]

  projected[, .(series_name, coicop, year, consumption, category)]
}

dt_estimated <- rbindlist(lapply(target_years, project_one_year), use.names = TRUE)

dt_total_estimated <- target_totals[
  year %in% target_years &
    is.finite(total_consumption) &
    total_consumption > 0 &
    coicop %in% dt_estimated[, unique(coicop)],
  .(
    series_name = "Italy HBS all-households total",
    coicop,
    year = as.numeric(year),
    total_consumption
  )
]

hbs_estimated <- hbs(
  dt = dt_estimated,
  dt_total = dt_total_estimated,
  country = "IT",
  category = "income",
  categories = hbs_it$categories,
  level = level
)

add_proxy_coicop_rows <- function(hbs_obj, mapping) {
  dt <- copy(hbs_obj$dt)
  dt_total <- copy(hbs_obj$dt_total)

  for (i in seq_len(nrow(mapping))) {
    child <- mapping$child[[i]]
    parent <- mapping$parent[[i]]

    if (!child %in% dt$coicop && parent %in% dt$coicop) {
      child_dt <- copy(dt[coicop == parent])
      child_dt[, coicop := child]
      if ("series_name" %in% names(child_dt)) {
        child_dt[, series_name := paste0(series_name, " (proxy from ", parent, ")")]
      } else {
        child_dt[, series_name := paste0("Italy HBS proxy from ", parent)]
      }
      dt <- rbindlist(list(dt, child_dt), use.names = TRUE, fill = TRUE)
    }

    if (!child %in% dt_total$coicop && parent %in% dt_total$coicop) {
      child_total <- copy(dt_total[coicop == parent])
      child_total[, coicop := child]
      if ("series_name" %in% names(child_total)) {
        child_total[, series_name := paste0(series_name, " (proxy from ", parent, ")")]
      } else {
        child_total[, series_name := paste0("Italy HBS total proxy from ", parent)]
      }
      dt_total <- rbindlist(list(dt_total, child_total), use.names = TRUE, fill = TRUE)
    }
  }

  hbs(
    dt = dt,
    dt_total = dt_total,
    country = hbs_obj$country,
    category = hbs_obj$category,
    categories = hbs_obj$categories,
    level = hbs_obj$level
  )
}

hbs_estimated <- add_proxy_coicop_rows(
  hbs_estimated,
  data.table(
    child = c("013", "023", "064", "074", "097", "098", "103", "122",
              "13", "131", "132", "133", "139"),
    parent = c("01", "02", "06", "07", "09", "09", "10", "12",
               "12", "12", "12", "12", "12")
  )
)

out_rds <- file.path(
  out_dir,
  "IT_income_hbs_estimated_2015_2020_from_2005_level2.rds"
)
saveRDS(hbs_estimated, out_rds)

diagnostics <- dt_estimated[, .(
  n_coicop = uniqueN(coicop),
  total_projected_consumption = sum(consumption, na.rm = TRUE)
), by = .(year, category)][order(year, category)]

coverage <- target_totals[, .(
  total_coicop_available = sum(is.finite(total_consumption) & total_consumption > 0),
  total_coicop_used = sum(
    is.finite(total_consumption) &
      total_consumption > 0 &
      coicop %in% dt_estimated[year == .BY$year, unique(coicop)]
  )
), by = year]

fwrite(
  diagnostics,
  file.path(out_dir, "IT_income_hbs_estimated_2015_2020_diagnostics.csv")
)
fwrite(
  coverage,
  file.path(out_dir, "IT_income_hbs_estimated_2015_2020_coverage.csv")
)

message("Wrote: ", normalizePath(out_rds, winslash = "/", mustWork = FALSE))
message("Rows in estimated HBS dt: ", nrow(hbs_estimated$dt))
message("Rows in estimated HBS dt_total: ", nrow(hbs_estimated$dt_total))
