suppressPackageStartupMessages({
  library(data.table)
  library(inflationinequality)
})

out_dir <- file.path("data-raw", "italy_hbs")
zip_dir <- file.path(out_dir, "zips")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

cfg <- list(
  years = c(2015L, 2020L),
  level = 1L,
  zips = c(
    "2015" = file.path(zip_dir, "HBS_2015_IT.zip"),
    "2020" = file.path(zip_dir, "HBS_2020_IT.zip")
  )
)

for (path in cfg$zips) {
  if (!file.exists(path)) stop("Missing HBS ZIP: ", path, call. = FALSE)
}

# Eurostat HBS_STR_T223, Italy 2005, unit = per mille, division level.
# Dataset: Structure of consumption expenditure by income quintile and COICOP
# consumption purpose, DOI https://doi.org/10.2908/HBS_STR_T223.
income_2005 <- data.table(
  coicop = rep(sprintf("CP%02d", 1:12), 5),
  quintile = rep(paste0("Q", 1:5), each = 12),
  income_pm = c(
    282, 23, 47, 361, 29, 29, 78, 31, 31, 2, 17, 71,
    236, 20, 61, 349, 35, 32, 89, 27, 41, 4, 30, 76,
    212, 19, 69, 328, 40, 35, 96, 24, 53, 6, 39, 79,
    187, 18, 75, 307, 49, 40, 98, 22, 61, 8, 53, 81,
    133, 14, 76, 239, 87, 46, 163, 16, 72, 9, 67, 78
  )
)

# Istat, Consumi delle famiglie 2005, Prospetto 1.34:
# "Spesa media mensile familiare per classe di quinti della spesa totale
# equivalente e capitoli di spesa - Anno 2005 (valori in euro e in percentuale)".
# Source PDF: https://ebiblio.istat.it/digibib/Consumi%20delle%20famiglie/Consumi_delle_famiglie_2005.pdf
expense_2005_istat <- data.table(
  chapter = c(
    "Alimentari e bevande",
    "Tabacchi",
    "Abbigliamento e calzature",
    "Abitazione",
    "Combustibili ed energia",
    "Mobili, elett. e servizi per la casa",
    "Sanita",
    "Trasporti",
    "Comunicazioni",
    "Istruzione",
    "Tempo libero, cultura e giochi",
    "Altri beni e servizi"
  ),
  Q1 = c(28.8, 1.4, 4.7, 27.7, 7.2, 3.4, 2.8, 11.8, 3.0, 0.6, 2.9, 5.7),
  Q2 = c(24.0, 1.1, 5.6, 28.3, 6.3, 3.9, 3.1, 12.4, 2.6, 0.9, 3.8, 7.9),
  Q3 = c(21.5, 0.9, 6.4, 27.2, 5.6, 4.6, 3.5, 12.7, 2.3, 0.9, 4.8, 9.5),
  Q4 = c(19.1, 0.8, 6.9, 26.0, 4.9, 5.6, 4.0, 12.3, 2.1, 1.2, 5.3, 11.8),
  Q5 = c(13.5, 0.6, 6.7, 23.6, 3.4, 8.7, 4.4, 17.6, 1.6, 1.1, 4.9, 14.0)
)

expense_long <- melt(
  expense_2005_istat,
  id.vars = "chapter",
  variable.name = "quintile",
  value.name = "expense_pct",
  variable.factor = FALSE
)
expense_long[, expense_pm := 10 * expense_pct]

# Alignment notes:
# - Istat's 2005 table separates housing and energy; Eurostat CP04 includes both.
# - Istat's "Alimentari e bevande" and "Tabacchi" are not a perfect split of
#   CP01 and CP02, so CP01 and CP02 are grouped before computing the bridge.
# - The 2005 Istat table has no separate restaurants/hotels chapter; travel and
#   holidays are discussed under "Altri beni e servizi", so CP11 and CP12 share
#   one bridge factor.
chapter_bridge <- data.table(
  chapter = expense_2005_istat$chapter,
  bridge_group = c(
    "CP01_CP02", "CP01_CP02", "CP03", "CP04", "CP04", "CP05",
    "CP06", "CP07", "CP08", "CP10", "CP09", "CP11_CP12"
  )
)
coicop_bridge <- data.table(
  coicop = sprintf("CP%02d", 1:12),
  bridge_group = c(
    "CP01_CP02", "CP01_CP02", "CP03", "CP04", "CP05", "CP06",
    "CP07", "CP08", "CP09", "CP10", "CP11_CP12", "CP11_CP12"
  )
)

expense_group_2005 <- merge(expense_long, chapter_bridge, by = "chapter")[
  ,
  .(expense_pm = sum(expense_pm)),
  by = .(quintile, bridge_group)
]
income_group_2005 <- merge(income_2005, coicop_bridge, by = "coicop")[
  ,
  .(income_pm = sum(income_pm)),
  by = .(quintile, bridge_group)
]
bridge_factor <- merge(
  income_group_2005,
  expense_group_2005,
  by = c("quintile", "bridge_group")
)
bridge_factor[, income_over_expense_factor := income_pm / expense_pm]

zip_member <- function(zip_file, pattern = "MICRODATI/.*\\.txt$") {
  members <- utils::unzip(zip_file, list = TRUE)
  hit <- members$Name[grepl(pattern, members$Name, ignore.case = TRUE)]
  if (length(hit) == 0L) {
    stop("No ZIP member matched pattern: ", pattern, call. = FALSE)
  }
  hit[[1L]]
}

fread_zip_member <- function(zip_file, member, select = NULL, ...) {
  tmp <- tempfile("istat_hbs_unzip_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)
  utils::unzip(zip_file, files = member, exdir = tmp, junkpaths = TRUE)
  fread(file.path(tmp, basename(member)), select = select, ...)
}

as_num <- function(x) suppressWarnings(as.numeric(x))

coalesce_col <- function(dt, candidates, default = NA_real_) {
  hit <- intersect(candidates, names(dt))
  if (length(hit) == 0L) return(rep(default, nrow(dt)))
  dt[[hit[[1L]]]]
}

carbonaro_scale <- function(n) {
  n <- as_num(n)
  fifelse(n <= 1, 0.60,
    fifelse(n == 2, 1.00,
      fifelse(n == 3, 1.33,
        fifelse(n == 4, 1.63,
          fifelse(n == 5, 1.90, fifelse(n == 6, 2.15, 2.40))
        )
      )
    )
  )
}

weighted_quintile <- function(x, w) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  out <- rep(NA_integer_, length(x))
  if (!any(ok)) return(out)
  ord <- order(x[ok], seq_along(x[ok]))
  p <- cumsum(w[ok][ord]) / sum(w[ok][ord])
  out[which(ok)[ord]] <- findInterval(p, c(0.2, 0.4, 0.6, 0.8)) + 1L
  out
}

division_cols <- function(names_vec) {
  cols <- c(
    "d_01", "d_02", "d_03", "d_04_str", "d_05", "d_06_aggr_1",
    "d_07", "d_08", "d_09", "d_10", "d_11", "d_12"
  )
  intersect(cols, names_vec)
}

division_map <- data.table(
  expense_var = c(
    "d_01", "d_02", "d_03", "d_04_str", "d_05", "d_06_aggr_1",
    "d_07", "d_08", "d_09", "d_10", "d_11", "d_12"
  ),
  coicop = sprintf("CP%02d", 1:12)
)

read_hbs_expense_quintile_year <- function(year, zip_file) {
  member <- zip_member(zip_file)
  nms <- names(fread_zip_member(zip_file, member, nrows = 0L))
  div_cols <- division_cols(nms)
  if (length(div_cols) != 12L) {
    stop("Expected 12 division columns for ", year, "; found ", length(div_cols), call. = FALSE)
  }
  needed <- intersect(c("w_anno", "c_Ncmp_altro", div_cols), nms)
  dt <- fread_zip_member(zip_file, member, select = needed)
  dt[, weight := as_num(coalesce_col(.SD, "w_anno", 1))]
  dt[!is.finite(weight) | weight <= 0, weight := 1]
  dt[, household_size := as_num(coalesce_col(.SD, "c_Ncmp_altro", 1))]
  dt[!is.finite(household_size) | household_size <= 0, household_size := 1]
  dt[, total_consumption := rowSums(.SD, na.rm = TRUE), .SDcols = div_cols]
  dt[, equivalised_consumption := total_consumption / carbonaro_scale(household_size)]
  dt[, expense_quintile := weighted_quintile(equivalised_consumption, weight)]

  long <- melt(
    dt,
    id.vars = c("weight", "expense_quintile"),
    measure.vars = div_cols,
    variable.name = "expense_var",
    value.name = "expenditure",
    variable.factor = FALSE
  )
  long[, expenditure := as_num(expenditure)]
  long <- long[is.finite(expenditure) & expenditure >= 0 & !is.na(expense_quintile)]
  long <- merge(long, division_map, by = "expense_var", all.x = TRUE)
  long <- merge(long, coicop_bridge, by = "coicop", all.x = TRUE)
  long[, expenditure_w := expenditure * weight]
  long[, year := as.integer(year)]

  expense_basket <- long[
    ,
    .(expense_consumption = sum(expenditure_w, na.rm = TRUE)),
    by = .(year, quintile = paste0("Q", expense_quintile), coicop, bridge_group)
  ]
  expense_basket[
    ,
    expense_share_pm := 1000 * expense_consumption / sum(expense_consumption),
    by = .(year, quintile)
  ]
  expense_basket[]
}

expense_targets <- rbindlist(lapply(names(cfg$zips), function(year) {
  read_hbs_expense_quintile_year(as.integer(year), cfg$zips[[year]])
}), use.names = TRUE)

corrected <- merge(
  expense_targets,
  bridge_factor[, .(quintile, bridge_group, income_over_expense_factor)],
  by = c("quintile", "bridge_group"),
  all.x = TRUE
)
corrected[, adjusted_raw := expense_consumption * income_over_expense_factor]
corrected[
  ,
  adjusted_share_pm := 1000 * adjusted_raw / sum(adjusted_raw, na.rm = TRUE),
  by = .(year, quintile)
]
corrected[, category := fifelse(
  quintile == "Q1", "First quintile",
  fifelse(quintile == "Q2", "Second quintile",
    fifelse(quintile == "Q3", "Third quintile",
      fifelse(quintile == "Q4", "Fourth quintile", "Fifth quintile")
    )
  )
)]

hbs_dt <- corrected[, .(
  series_name = "Italy HBS income targets from expense-quintile bridge",
  coicop = sub("^CP", "", coicop),
  year = as.numeric(year),
  consumption = adjusted_share_pm,
  category
)]

hbs_total <- expense_targets[
  ,
  .(total_consumption = sum(expense_consumption, na.rm = TRUE)),
  by = .(year, coicop)
]
hbs_total[
  ,
  total_consumption := 1000 * total_consumption / sum(total_consumption),
  by = year
]
hbs_total <- hbs_total[, .(
  series_name = "Italy HBS total by expense microdata",
  coicop = sub("^CP", "", coicop),
  year = as.numeric(year),
  total_consumption
)]

hbs_obj <- hbs(
  dt = hbs_dt,
  dt_total = hbs_total,
  country = "IT",
  category = "income",
  categories = c(
    "First quintile", "Second quintile", "Third quintile",
    "Fourth quintile", "Fifth quintile"
  ),
  level = cfg$level
)

fwrite(
  bridge_factor,
  file.path(out_dir, "IT_2005_income_over_expense_quintile_bridge_factor.csv")
)
fwrite(
  corrected[
    ,
    .(
      year, quintile, category, coicop, bridge_group,
      expense_share_pm, income_over_expense_factor, adjusted_share_pm
    )
  ],
  file.path(out_dir, "IT_income_targets_from_expense_bridge_2015_2020_level1.csv")
)
income_group_shares <- corrected[
  ,
  .(adjusted_total = sum(adjusted_raw, na.rm = TRUE)),
  by = .(year, quintile, category)
]
income_group_shares[
  ,
  share_total := 100 * adjusted_total / sum(adjusted_total),
  by = year
]
fwrite(
  income_group_shares[, .(
    Dimension = "Income",
    Group = category,
    year = as.numeric(year),
    share_total
  )],
  file.path(out_dir, "IT_income_group_shares_from_expense_bridge_2015_2020.csv")
)
saveRDS(
  hbs_obj,
  file.path(out_dir, "IT_income_targets_from_expense_bridge_2015_2020_level1.rds")
)

message("Wrote bridge factor, corrected targets, and income group shares to ", normalizePath(out_dir, winslash = "/", mustWork = FALSE))
