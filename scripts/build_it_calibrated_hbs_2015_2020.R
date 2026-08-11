suppressPackageStartupMessages({
  library(data.table)
  library(inflationinequality)
})

repo <- "C:/Users/jerem/Documents/GitHub/inflationinequality"
out_dir <- file.path(repo, "data-raw", "italy_calibrated_hbs")
ext_dir <- file.path(repo, "inst", "extdata")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

years <- c(2015L, 2020L)
quintiles <- c(
  "First quintile", "Second quintile", "Third quintile",
  "Fourth quintile", "Fifth quintile"
)

target_file <- file.path(
  repo, "data-raw", "italy_hbs",
  "IT_income_targets_from_expense_bridge_2015_2020_level1.csv"
)
age_file <- file.path(
  repo, "data-raw", "italy_hbs",
  "IT_age_hbs_eurostat_2015_2020_level2_uncalibrated.rds"
)
stopifnot(file.exists(target_file), file.exists(age_file))

targets <- fread(target_file)
targets <- targets[year %in% years]
targets[, division := sub("^CP", "", coicop)]

# Italian national COICOP-3 margins from Eurostat HBS. The package calls
# three-digit codes (e.g. 041, 042) level 2.
age_hbs <- readRDS(age_file)
legacy_coicop3 <- c(
  "011", "012", "021", "022", "023", "031", "032",
  "041", "042", "043", "044", "045",
  "051", "052", "053", "054", "055", "056",
  "061", "062", "063", "071", "072", "073",
  "081", "082", "083", "091", "092", "093", "094", "095", "096",
  "101", "102", "103", "104", "105", "111", "112",
  "121", "122", "123", "124", "125", "126", "127"
)
age_dt <- copy(age_hbs$dt)[year %in% years & (nchar(coicop) == 2L | coicop %in% legacy_coicop3)]
age_total <- copy(age_hbs$dt_total)[year %in% years & (nchar(coicop) == 2L | coicop %in% legacy_coicop3)]
# Published per-mille cells are rounded. Normalize the COICOP-3 cells and
# reconstruct division rows so every basket is internally additive.
age_child <- age_dt[nchar(coicop) == 3L]
age_child[, consumption := 1000 * consumption / sum(consumption), by = .(year, category)]
age_parent <- age_child[, .(
  series_name = paste("IT.age.cleaned", year, category, substr(coicop[1L], 1L, 2L), sep = "."),
  coicop = substr(coicop[1L], 1L, 2L),
  consumption = sum(consumption)
), by = .(year, category, division = substr(coicop, 1L, 2L))][, division := NULL]
age_dt <- rbindlist(list(age_parent, age_child), use.names = TRUE)

total_child <- age_total[nchar(coicop) == 3L]
total_child[, total_consumption := 1000 * total_consumption / sum(total_consumption), by = year]
total_parent <- total_child[, .(
  series_name = paste("IT.age.cleaned.total", year, substr(coicop[1L], 1L, 2L), sep = "."),
  coicop = substr(coicop[1L], 1L, 2L),
  total_consumption = sum(total_consumption)
), by = .(year, division = substr(coicop, 1L, 2L))][, division := NULL]
age_total <- rbindlist(list(total_parent, total_child), use.names = TRUE)
age_hbs <- inflationinequality::hbs(
  dt = age_dt, dt_total = age_total, country = "IT", category = "age",
  categories = age_hbs$categories, level = 2
)
national <- copy(age_hbs$dt_total)
national[, division := substr(coicop, 1L, 2L)]

children <- national[nchar(coicop) == 3L]
children[, within_division := total_consumption / sum(total_consumption), by = .(year, division)]
if (children[, any(!is.finite(within_division))]) stop("Invalid Italian COICOP-3 margins")

# The 2005 income-to-expenditure bridge overstates selected gradients when
# projected to 2020. Contract only housing, transport and recreation toward
# the contemporaneous Italian national HBS margin. CP041 and CP042 remain in
# CP04; downstream, the package maps their combined expenditure to the CP041
# rent price index (rental-equivalence convention).
gradient_lambda <- data.table(
  division = sprintf("%02d", 1:12),
  # Conservative income specification: all bridge gradients are shrunk by
  # 35%, with stronger shrinkage for the three divisions previously flagged.
  lambda = c(0.65, 0.65, 0.65, 0.20, 0.65, 0.65,
             0.35, 0.65, 0.35, 0.65, 0.65, 0.65)
)
national_division <- national[nchar(coicop) == 2L, .(
  year, division = coicop, national_raw = total_consumption
)]
national_division[, national_share := national_raw / sum(national_raw), by = year]
targets[, adjusted_share_pm_uncontracted := adjusted_share_pm]
targets <- merge(targets, national_division[, .(year, division, national_share)],
  by = c("year", "division"), all.x = TRUE)
targets <- merge(targets, gradient_lambda, by = "division", all.x = TRUE)
targets[, bridge_share := adjusted_share_pm_uncontracted / sum(adjusted_share_pm_uncontracted),
  by = .(year, category)]
targets[, contracted_raw := national_share + lambda * (bridge_share - national_share)]
targets[, adjusted_share_pm := 1000 * contracted_raw / sum(contracted_raw),
  by = .(year, category)]
if (targets[, any(!is.finite(adjusted_share_pm))]) stop("Invalid contracted income targets")

build_level2 <- function(value_col, label) {
  div <- targets[, .(
    year = as.numeric(year),
    division,
    category,
    value = as.numeric(get(value_col))
  )]
  div[, value := 1000 * value / sum(value), by = .(year, category)]

  child <- merge(div, children[, .(year, division, coicop, within_division)],
    by = c("year", "division"), all.x = TRUE, allow.cartesian = TRUE)
  if (child[is.na(coicop), .N]) stop("Missing Italian COICOP-3 decomposition")
  child[, consumption := value * within_division]
  child[, series_name := paste(label, year, category, coicop, sep = ".")]

  parent <- div[, .(
    series_name = paste(label, year, category, division, sep = "."),
    coicop = division,
    year,
    category,
    consumption = value
  )]
  dt <- rbindlist(list(
    parent,
    child[, .(series_name, coicop, year, category, consumption)]
  ))
  setorder(dt, year, category, coicop)

  total <- copy(national)[, .(
    series_name = paste(label, "total", year, coicop, sep = "."),
    coicop,
    year = as.numeric(year),
    total_consumption = as.numeric(total_consumption)
  )]
  inflationinequality::hbs(
    dt = dt, dt_total = total, country = "IT", category = "income",
    categories = quintiles, level = 2
  )
}

# Main specification: 2005 Italian income structure bridged with Italian
# expenditure-quintile movements in 2015/2020, then decomposed using Italian
# national COICOP-3 margins. CP041 and CP042 are both retained.
income_hbs <- build_level2("adjusted_share_pm", "IT.income.calibrated")

# Observed consumption-expenditure quintiles from Italian HBS microdata.
consumption_hbs <- build_level2("expense_share_pm", "IT.consumption.quintile")

validate_sums <- function(x, object_name) {
  d <- copy(x$dt)[nchar(coicop) == 3L, .(
    sum_pm = sum(consumption),
    n_coicop3 = uniqueN(coicop),
    has_041 = "041" %in% coicop,
    has_042 = "042" %in% coicop
  ), by = .(year, category)]
  d[, `:=`(
    object = object_name,
    error_from_1000 = sum_pm - 1000,
    valid = abs(sum_pm - 1000) < 1e-5 & n_coicop3 == 47L & has_041 & has_042
  )]
  d
}

diag <- rbindlist(list(
  validate_sums(income_hbs, "income_calibrated"),
  validate_sums(age_hbs, "age_eurostat"),
  validate_sums(consumption_hbs, "consumption_quintile")
), fill = TRUE)

# Donatiello et al. (2022), Table 4.4. These moments scale total consumption
# relative to income; they do not alter within-quintile COICOP shares.
donatiello <- data.table(
  category = quintiles,
  consumption_to_income = c(1.61, 1.03, 0.93, 0.83, 0.59)
)
group_totals <- unique(targets[, .(year, category, quintile)])
group_totals <- merge(group_totals, donatiello, by = "category", all.x = TRUE)
group_totals[, relative_income := 1 / consumption_to_income]
group_totals[, relative_income_share := relative_income / sum(relative_income), by = year]
group_totals[, implied_consumption := relative_income_share * consumption_to_income]
group_totals[, implied_consumption_share := implied_consumption / sum(implied_consumption), by = year]
group_totals[, reproduced_ratio := implied_consumption / relative_income_share]

saveRDS(consumption_hbs, file.path(out_dir, "IT_consumption_quintile_hbs_2015_2020_level2.rds"))
saveRDS(income_hbs, file.path(ext_dir, "IT_income_hbs_calibrated_2015_2020_level2.rds"))
saveRDS(consumption_hbs, file.path(ext_dir, "IT_consumption_quintile_hbs_2015_2020_level2.rds"))
saveRDS(age_hbs, file.path(ext_dir, "IT_age_hbs_eurostat_2015_2020_level2.rds"))

fwrite(income_hbs$dt[nchar(coicop) == 3L],
  file.path(out_dir, "IT_income_baskets_2015_2020_COICOP3.csv"))
fwrite(consumption_hbs$dt[nchar(coicop) == 3L],
  file.path(out_dir, "IT_consumption_quintile_baskets_2015_2020_COICOP3.csv"))
fwrite(diag, file.path(out_dir, "IT_hbs_structure_diagnostics.csv"))
fwrite(group_totals, file.path(out_dir, "IT_donatiello_moment_diagnostics.csv"))
fwrite(targets[, .(
  year, category, division, national_share, lambda,
  adjusted_share_pm_uncontracted, adjusted_share_pm
)], file.path(out_dir, "IT_income_gradient_contraction_diagnostics.csv"))

if (!diag[, all(valid)]) {
  print(diag[valid == FALSE])
  stop("At least one Italian HBS structure failed validation")
}
if (max(abs(group_totals$reproduced_ratio - group_totals$consumption_to_income)) > 1e-12) {
  stop("Donatiello moments were not reproduced")
}

message("Italian income, age and consumption-quintile HBS structures validated.")
