suppressPackageStartupMessages({
  library(data.table)
  library(inflationinequality)
})

istat_dir <- Sys.getenv(
  "ISTAT_HBS_DIR",
  "C:/Users/jerem/Documents/New project/data/istat_hbs"
)
out_dir <- file.path("data-raw", "italy_hbs")
zip_out_dir <- file.path(out_dir, "zips")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(zip_out_dir, recursive = TRUE, showWarnings = FALSE)

years <- c(2015L, 2020L)

ensure_repo_zip <- function(file_name) {
  repo_zip <- file.path(zip_out_dir, file_name)
  if (file.exists(repo_zip)) {
    return(repo_zip)
  }
  source_zip <- file.path(istat_dir, file_name)
  if (!file.exists(source_zip)) {
    stop("Missing Istat ZIP: ", source_zip, call. = FALSE)
  }
  file.copy(source_zip, repo_zip, overwrite = TRUE)
  repo_zip
}

source_zips <- file.path(zip_out_dir, sprintf("HBS_%s_IT.zip", years))
names(source_zips) <- as.character(years)
source_zips[] <- vapply(basename(source_zips), ensure_repo_zip, character(1))

zip_member <- function(zip_file, pattern = "MICRODATI/.*\\.txt$") {
  members <- utils::unzip(zip_file, list = TRUE)
  hit <- members$Name[grepl(pattern, members$Name, ignore.case = TRUE)]
  if (length(hit) == 0L) {
    stop("No ZIP member matched pattern: ", pattern, call. = FALSE)
  }
  hit[[1L]]
}

fread_zip_member <- function(zip_file, member, select = NULL, ...) {
  out_dir <- tempfile("istat_hbs_unzip_")
  dir.create(out_dir)
  on.exit(unlink(out_dir, recursive = TRUE, force = TRUE), add = TRUE)
  utils::unzip(zip_file, files = member, exdir = out_dir, junkpaths = TRUE)
  extracted <- file.path(out_dir, basename(member))
  data.table::fread(extracted, select = select, ...)
}

as_num <- function(x) suppressWarnings(as.numeric(x))

age_group <- function(age_code) {
  code <- as.integer(as.character(age_code))
  fifelse(code == 1L, "Under 18 years",
    fifelse(code == 2L, "18--34 years",
      fifelse(code == 3L, "35--64 years",
        fifelse(code == 4L, "65 years or over", NA_character_)
      )
    )
  )
}

read_age_year <- function(year, zip_file) {
  member <- zip_member(zip_file)
  header <- fread_zip_member(zip_file, member, nrows = 0L)
  available <- names(header)
  expense_map <- c(
    "01" = "d_01",
    "02" = "d_02",
    "03" = "d_03",
    "04" = "d_04_str",
    "05" = "d_05",
    "06" = "d_06_aggr_1",
    "07" = "d_07",
    "08" = "d_08",
    "09" = "d_09",
    "10" = "d_10",
    "11" = "d_11",
    "12" = "d_12"
  )
  expense_map <- expense_map[expense_map %in% available]
  select <- unique(c("w_anno", "c_c_etacalc_1", unname(expense_map)))
  dt <- fread_zip_member(zip_file, member, select = select)
  dt[, weight := as_num(w_anno)]
  dt[!is.finite(weight) | weight <= 0, weight := 1]
  dt[, category := age_group(c_c_etacalc_1)]
  dt <- dt[!is.na(category)]
  dt[, household_consumption := rowSums(.SD, na.rm = TRUE), .SDcols = unname(expense_map)]
  dt[, household_consumption_w := household_consumption * weight]

  group_total_shares <- dt[, .(
    share_total = 100 * sum(household_consumption_w, na.rm = TRUE) /
      sum(dt$household_consumption_w, na.rm = TRUE)
  ), by = category]
  group_total_shares[, `:=`(
    Dimension = "Age",
    Group = category,
    year = as.numeric(year)
  )]
  group_total_shares <- group_total_shares[, .(Dimension, Group, year, share_total)]

  long <- melt(
    dt,
    id.vars = c("weight", "category"),
    measure.vars = unname(expense_map),
    variable.name = "expense_var",
    value.name = "expenditure",
    variable.factor = FALSE
  )
  inv_map <- setNames(names(expense_map), unname(expense_map))
  long[, coicop := inv_map[expense_var]]
  long[, expenditure := as_num(expenditure)]
  long <- long[is.finite(expenditure) & expenditure > 0]
  long[, expenditure_w := expenditure * weight]

  dt_hbs <- long[, .(
    consumption = sum(expenditure_w, na.rm = TRUE)
  ), by = .(coicop, category)]
  dt_hbs[, `:=`(
    series_name = sprintf("Istat HBS original ECOICOP v1 age %s", year),
    year = as.numeric(year)
  )]
  dt_hbs <- dt_hbs[, .(series_name, coicop, year, consumption, category)]

  dt_total <- long[, .(
    total_consumption = sum(expenditure_w, na.rm = TRUE)
  ), by = coicop]
  dt_total[, `:=`(
    series_name = sprintf("Istat HBS original ECOICOP v1 total %s", year),
    year = as.numeric(year)
  )]
  dt_total <- dt_total[, .(series_name, coicop, year, total_consumption)]

  list(dt = dt_hbs, dt_total = dt_total, group_total_shares = group_total_shares)
}

age_parts <- lapply(names(source_zips), function(year) {
  read_age_year(as.integer(year), source_zips[[year]])
})

age_categories <- c(
  "Under 18 years",
  "18--34 years",
  "35--64 years",
  "65 years or over"
)

hbs_age <- hbs(
  dt = rbindlist(lapply(age_parts, `[[`, "dt"), use.names = TRUE),
  dt_total = rbindlist(lapply(age_parts, `[[`, "dt_total"), use.names = TRUE),
  country = "IT",
  category = "age",
  categories = age_categories,
  level = 1
)

# The Istat public-use HBS files used above do not expose the Eurostat
# degree-of-urbanisation variable. For residence-area groups we therefore keep
# the harmonised Eurostat HBS table, which provides Rural areas, Towns and
# suburbs, and Cities for Italy in 2015 and 2020.
hbs_urban <- load_hbs(
  "IT",
  "urban",
  level = 2,
  start_year = min(years),
  end_year = max(years)
)
hbs_age_level2 <- load_hbs(
  "IT",
  "age",
  level = 2,
  start_year = min(years),
  end_year = max(years)
)

age_path <- file.path(out_dir, "IT_age_hbs_istat_original_2015_2020_level1.rds")
age_level2_path <- file.path(out_dir, "IT_age_hbs_eurostat_2015_2020_level2.rds")
urban_path <- file.path(out_dir, "IT_urban_hbs_eurostat_2015_2020_level2.rds")
saveRDS(hbs_age, age_path)
saveRDS(hbs_age_level2, age_level2_path)
saveRDS(hbs_urban, urban_path)

fwrite(
  hbs_age$dt[, .(consumption = sum(consumption)), by = .(year, category)],
  file.path(out_dir, "IT_age_hbs_istat_original_2015_2020_diagnostics.csv")
)
fwrite(
  hbs_urban$dt[, .(consumption = sum(consumption)), by = .(year, category)],
  file.path(out_dir, "IT_urban_hbs_eurostat_2015_2020_diagnostics.csv")
)
fwrite(
  hbs_age_level2$dt[, .(consumption = sum(consumption)), by = .(year, category)],
  file.path(out_dir, "IT_age_hbs_eurostat_2015_2020_level2_diagnostics.csv")
)

income_anchor <- data.table(
  Dimension = "Income",
  Group = c(
    "First quintile", "Second quintile", "Third quintile",
    "Fourth quintile", "Fifth quintile"
  ),
  share_total = c(14.6, 17.8, 20.0, 22.8, 24.7)
)[
  ,
  .(year = c(2005, 2015, 2020), share_total),
  by = .(Dimension, Group)
]
urban_blank <- data.table(
  Dimension = "Residence area",
  Group = rep(c("Rural areas", "Towns and suburbs", "Cities"), each = 3L),
  year = rep(c(2005, 2015, 2020), times = 3L),
  share_total = as.numeric(NA)
)
group_shares <- rbindlist(
  list(
    income_anchor,
    rbindlist(lapply(age_parts, `[[`, "group_total_shares"), use.names = TRUE),
    urban_blank
  ),
  use.names = TRUE
)
fwrite(
  group_shares,
  file.path(out_dir, "IT_hbs_all_products_group_shares_2005_2015_2020.csv")
)

message("Wrote: ", normalizePath(age_path, winslash = "/", mustWork = FALSE))
message("Wrote: ", normalizePath(age_level2_path, winslash = "/", mustWork = FALSE))
message("Wrote: ", normalizePath(urban_path, winslash = "/", mustWork = FALSE))
message("Istat ZIPs available in: ", normalizePath(zip_out_dir, winslash = "/", mustWork = FALSE))
