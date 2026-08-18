suppressPackageStartupMessages({
  library(data.table)
  library(readxl)
})

file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_file <- if (length(file_arg) > 0L) {
  normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = TRUE)
} else {
  normalizePath("scripts/build_pt_idef_2015_hbs.R", mustWork = TRUE)
}
root <- normalizePath(file.path(dirname(script_file), ".."), mustWork = TRUE)
if (!exists("hbs", mode = "function")) {
  if (!requireNamespace("pkgload", quietly = TRUE)) {
    stop("Package 'pkgload' is required to build the compact Portugal IDEF HBS objects.")
  }
  pkgload::load_all(root, quiet = TRUE)
}

out_dir <- file.path(root, "data-raw", "portugal_idef")
ext_dir <- file.path(root, "inst", "extdata")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(ext_dir, recursive = TRUE, showWarnings = FALSE)

results_url <- paste0(
  "https://www.ine.pt/ngt_server/attachfileu.jsp?",
  "look_parentBoui=305268801&att_display=n&att_download=y"
)
coicop4_url <- paste0(
  "https://www.ine.pt/ngt_server/attachfileu.jsp?",
  "look_parentBoui=305274094&att_display=n&att_download=y"
)

source_file <- function(env_name, default_name, url) {
  path <- Sys.getenv(env_name, unset = "")
  if (!nzchar(path)) {
    path <- file.path(out_dir, default_name)
  }
  if (!file.exists(path)) {
    message("Downloading ", url)
    utils::download.file(url, path, mode = "wb", quiet = TRUE)
  }
  normalizePath(path, mustWork = TRUE)
}

results_path <- source_file(
  "PT_IDEF_RESULTS_XLSX", "IDEF_2015_2016_results.xlsx", results_url
)
coicop4_path <- source_file(
  "PT_IDEF_COICOP4_XLSX", "IDEF_2015_2016_COICOP4.xlsx", coicop4_url
)

clean_code <- function(x) {
  x <- gsub("[^0-9]", "", as.character(x))
  x[nchar(x) < 2L | nchar(x) > 4L] <- NA_character_
  x
}

numeric_value <- function(x) {
  out <- suppressWarnings(as.numeric(x))
  out[!is.finite(out)] <- NA_real_
  out
}

read_national_coicop <- function(path) {
  sheets <- excel_sheets(path)
  sheet <- grep("^IDEF2015 \\(Nuts II_", sheets)[1L]
  raw <- as.data.table(read_excel(
    path,
    sheet = sheet,
    col_names = FALSE,
    .name_repair = "minimal"
  ))
  setnames(raw, paste0("V", seq_along(raw)))
  out <- raw[, .(
    coicop = clean_code(V1),
    label = as.character(V2),
    total_consumption = numeric_value(V3)
  )]
  out <- out[!is.na(coicop)]
  unique(out, by = "coicop")
}

fill_masked_children <- function(dt, child_length, value_col) {
  parent_length <- child_length - 1L
  children <- dt[nchar(coicop) == child_length]
  parents <- dt[nchar(coicop) == parent_length, .(
    parent = coicop,
    parent_value = get(value_col)
  )]
  children[, parent := substr(coicop, 1L, parent_length)]
  children <- parents[children, on = "parent"]

  children[, (value_col) := {
    values <- get(value_col)
    missing <- is.na(values) | values <= 0
    if (any(missing)) {
      residual <- max(parent_value[[1L]] - sum(values[!missing], na.rm = TRUE), 0)
      values[missing] <- max(residual / sum(missing), 1e-6)
    }
    values
  }, by = parent]

  dt[children, on = "coicop", (value_col) := get(paste0("i.", value_col))]
  dt
}

national <- read_national_coicop(coicop4_path)
national <- fill_masked_children(national, 3L, "total_consumption")
national <- fill_masked_children(national, 4L, "total_consumption")
national[is.na(total_consumption) | total_consumption <= 0, total_consumption := 1e-6]

national3 <- national[nchar(coicop) == 3L, .(
  coicop,
  national_parent = total_consumption
)]
national4 <- national[nchar(coicop) == 4L, .(
  coicop,
  parent = substr(coicop, 1L, 3L),
  total_consumption
)]
national4[, leaf_sum := sum(total_consumption), by = parent]
national4[, national_leaf_share := total_consumption / leaf_sum]

read_group_table <- function(sheet_prefix, value_columns, categories) {
  sheets <- excel_sheets(results_path)
  sheet <- sheets[startsWith(sheets, sheet_prefix) & !grepl("%", sheets, fixed = TRUE)][1L]
  if (is.na(sheet)) {
    stop("Could not locate IDEF result sheet beginning with '", sheet_prefix, "'.")
  }
  raw <- as.data.table(read_excel(
    results_path,
    sheet = sheet,
    col_names = FALSE,
    .name_repair = "minimal"
  ))
  setnames(raw, paste0("V", seq_along(raw)))
  codes <- clean_code(raw[[1L]])
  keep <- !is.na(codes) & nchar(codes) %in% c(2L, 3L)
  raw <- raw[keep]
  codes <- codes[keep]

  dt <- rbindlist(lapply(seq_along(value_columns), function(i) {
    data.table(
      coicop = codes,
      category = categories[[i]],
      consumption = numeric_value(raw[[value_columns[[i]]]])
    )
  }))
  unique(dt, by = c("coicop", "category"))
}

fill_group_masked_level3 <- function(dt) {
  level3 <- dt[nchar(coicop) == 3L]
  level3[, division := substr(coicop, 1L, 2L)]
  divisions <- dt[nchar(coicop) == 2L, .(
    division = coicop,
    category,
    division_consumption = consumption
  )]
  level3 <- divisions[level3, on = .(division, category)]
  level3 <- national3[level3, on = "coicop"]

  level3[, consumption := {
    values <- consumption
    missing <- is.na(values) | values <= 0
    if (any(missing)) {
      residual <- max(division_consumption[[1L]] - sum(values[!missing], na.rm = TRUE), 0)
      weights <- national_parent[missing]
      if (all(!is.finite(weights)) || sum(weights, na.rm = TRUE) <= 0) {
        weights <- rep(1, sum(missing))
      }
      values[missing] <- pmax(residual * weights / sum(weights, na.rm = TRUE), 1e-6)
    }
    values
  }, by = .(division, category)]

  dt[level3, on = .(coicop, category), consumption := i.consumption]
  dt[is.na(consumption) | consumption <= 0, consumption := 1e-6]
  dt
}

make_hbs <- function(category_name, group_dt, categories) {
  group_dt <- fill_group_masked_level3(group_dt)
  group3 <- group_dt[nchar(coicop) == 3L, .(
    parent = coicop,
    category,
    parent_consumption = consumption
  )]
  leaves <- group3[national4, on = "parent", allow.cartesian = TRUE, nomatch = 0L]
  leaves[, consumption := pmax(parent_consumption * national_leaf_share, 1e-6)]
  leaves <- leaves[, .(coicop, category, consumption)]

  dt <- rbindlist(
    list(group_dt[, .(coicop, category, consumption)], leaves),
    use.names = TRUE
  )
  dt[, `:=`(
    series_name = paste("PT IDEF 2015/2016", category_name, coicop, category, sep = "."),
    year = 2015L
  )]
  setcolorder(dt, c("series_name", "coicop", "year", "category", "consumption"))
  setorder(dt, coicop, category)

  dt_total <- national[, .(
    series_name = paste("PT IDEF 2015/2016", coicop, "Total", sep = "."),
    coicop,
    year = 2015L,
    total_consumption
  )]
  dt_total <- dt_total[coicop %in% dt$coicop]
  setorder(dt_total, coicop)

  hbs(
    dt = dt,
    dt_total = dt_total,
    country = "PT",
    category = category_name,
    categories = categories,
    level = 3
  )
}

specs <- list(
  income = list(
    sheet = "Q 2.8 (",
    columns = c(5L, 7L, 9L, 11L, 13L),
    categories = category_data$income$categories
  ),
  age = list(
    sheet = "Q 2.10 (",
    columns = c(5L, 7L, 9L, 11L),
    categories = category_data$age$categories
  ),
  urban = list(
    sheet = "Q 2.2 (",
    columns = c(9L, 7L, 5L),
    categories = category_data$urban$categories
  )
)

objects <- lapply(names(specs), function(category_name) {
  spec <- specs[[category_name]]
  grouped <- read_group_table(spec$sheet, spec$columns, spec$categories)
  make_hbs(category_name, grouped, spec$categories)
})
names(objects) <- names(specs)

diagnostics <- rbindlist(lapply(names(objects), function(category_name) {
  obj <- objects[[category_name]]
  leaf_check <- obj$dt[nchar(coicop) == 4L, .(
    leaf_sum = sum(consumption)
  ), by = .(category, parent = substr(coicop, 1L, 3L))]
  parent_check <- obj$dt[nchar(coicop) == 3L, .(
    category,
    parent = coicop,
    parent_consumption = consumption
  )]
  check <- parent_check[leaf_check, on = .(category, parent)]
  data.table(
    source = "INE IDEF 2015/2016 published tables",
    method = "Published group COICOP3 totals allocated by national COICOP4 shares",
    category = category_name,
    n_categories = length(obj$categories),
    n_coicop4 = uniqueN(obj$dt[nchar(coicop) == 4L, coicop]),
    max_parent_leaf_gap = max(abs(check$parent_consumption - check$leaf_sum)),
    output_rds = portugal_idef_2015_hbs_level3_file_name(category_name)
  )
}), use.names = TRUE)

for (category_name in names(objects)) {
  file_name <- portugal_idef_2015_hbs_level3_file_name(category_name)
  data_raw_path <- file.path(out_dir, file_name)
  ext_path <- file.path(ext_dir, file_name)
  saveRDS(objects[[category_name]], data_raw_path, compress = "xz")
  saveRDS(objects[[category_name]], ext_path, compress = "xz")
  message("Wrote ", normalizePath(data_raw_path, winslash = "/", mustWork = FALSE))
  message("Wrote ", normalizePath(ext_path, winslash = "/", mustWork = FALSE))
}

fwrite(diagnostics, file.path(out_dir, "PT_idef_2015_level3_diagnostics.csv"))
