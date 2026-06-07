suppressPackageStartupMessages({
  library(data.table)
})

file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_file <- if (length(file_arg) > 0L) {
  normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = TRUE)
} else {
  normalizePath("scripts/build_es_epf_2020_hbs.R", mustWork = TRUE)
}
root <- normalizePath(file.path(dirname(script_file), ".."), mustWork = TRUE)
if (!exists("hbs", mode = "function")) {
  if (!requireNamespace("pkgload", quietly = TRUE)) {
    stop("Package 'pkgload' is required to build the compact Spain EPF HBS objects.")
  }
  pkgload::load_all(root, quiet = TRUE)
}

out_dir <- file.path(root, "data-raw", "spain_epf")
ext_dir <- file.path(root, "inst", "extdata")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(ext_dir, recursive = TRUE, showWarnings = FALSE)

zip_path <- Sys.getenv("ES_EPF_2020_ZIP", unset = "")
if (!nzchar(zip_path)) {
  zip_path <- file.path(Sys.getenv("TEMP"), "ine_epf_2020.zip")
}
if (!file.exists(zip_path)) {
  stop(
    "EPF 2020 ZIP not found. Download INE datos_2020.zip and either ",
    "place it at %TEMP%/ine_epf_2020.zip or set ES_EPF_2020_ZIP."
  )
}

work_dir <- file.path(tempdir(), "ine_epf_2020_hbs_build")
unlink(work_dir, recursive = TRUE, force = TRUE)
dir.create(work_dir, recursive = TRUE)
on.exit(unlink(work_dir, recursive = TRUE, force = TRUE), add = TRUE)

utils::unzip(zip_path, files = c("EPFgastos_2020.zip", "EPFhogar_2020.zip"), exdir = work_dir)
utils::unzip(file.path(work_dir, "EPFgastos_2020.zip"), exdir = file.path(work_dir, "gastos"))
utils::unzip(file.path(work_dir, "EPFhogar_2020.zip"), exdir = file.path(work_dir, "hogar"))

households <- fread(
  file.path(work_dir, "hogar", "CSV", "EPFhogar_2020.csv"),
  sep = "\t",
  select = c(
    "ANOENC", "NUMERO", "FACTOR", "UC1", "IMPEXAC",
    "EDADSP", "DENSIDAD"
  ),
  na.strings = c("", "NA")
)
households <- households[!is.na(NUMERO) & !is.na(FACTOR) & FACTOR > 0]
households[, household_id := paste(ANOENC, NUMERO, sep = "_")]

households[!is.na(UC1) & UC1 > 0 & !is.na(IMPEXAC) & IMPEXAC > 0,
  equivalised_income := IMPEXAC / UC1
]
setorder(households, equivalised_income, household_id)
households[is.finite(equivalised_income), income_rank_weight := cumsum(FACTOR)]
households[is.finite(equivalised_income), income_total_weight := sum(FACTOR)]
households[is.finite(equivalised_income),
  income_group := paste0(
    "Decile ",
    pmin(10L, pmax(1L, ceiling(10 * income_rank_weight / income_total_weight)))
  )
]

households[, age_group := fifelse(EDADSP < 30, "Less than 30 years",
  fifelse(EDADSP < 45, "From 30 to 44 years",
    fifelse(EDADSP < 60, "From 45 to 59 years",
      fifelse(EDADSP >= 60, "60 years or over", NA_character_)
    )
  )
)]

households[, urban_group := fifelse(DENSIDAD == 3L, "Rural areas",
  fifelse(DENSIDAD == 2L, "Towns and suburbs",
    fifelse(DENSIDAD == 1L, "Cities", NA_character_)
  )
)]

expenses <- fread(
  file.path(work_dir, "gastos", "CSV", "EPFgastos_2020.csv"),
  sep = "\t",
  select = c("ANOENC", "NUMERO", "CODIGO", "GASTO"),
  na.strings = c("", "NA")
)
expenses <- expenses[!is.na(CODIGO) & !is.na(GASTO) & GASTO >= 0]
expenses[, `:=`(
  household_id = paste(ANOENC, NUMERO, sep = "_"),
  coicop = substr(CODIGO, 1, 4),
  gasto_eur = GASTO / 1000
)]
expenses <- expenses[nchar(coicop) == 4]

make_hbs <- function(category_name, group_col, categories) {
  hh <- households[!is.na(get(group_col)), .(household_id, FACTOR, category = get(group_col))]
  group_weights <- hh[, .(group_weight = sum(FACTOR)), by = category]
  total_weight <- hh[, sum(FACTOR)]

  exp_grouped <- expenses[
    hh,
    on = "household_id",
    nomatch = 0
  ]

  coicops <- sort(unique(exp_grouped$coicop))
  skeleton <- CJ(coicop = coicops, category = categories, unique = TRUE)

  dt <- exp_grouped[
    ,
    .(weighted_spend = sum(gasto_eur * FACTOR, na.rm = TRUE)),
    by = .(coicop, category)
  ][
    skeleton,
    on = .(coicop, category)
  ][
    group_weights,
    on = "category"
  ]
  dt[is.na(weighted_spend), weighted_spend := 0]
  dt[, consumption := weighted_spend / group_weight]
  dt[, `:=`(
    series_name = paste("ES EPF 2020", category_name, coicop, category, sep = "."),
    year = 2020
  )]
  dt <- dt[, .(series_name, coicop, year, category, consumption)]
  setorder(dt, coicop, category)

  dt_total <- exp_grouped[
    ,
    .(total_consumption = sum(gasto_eur * FACTOR, na.rm = TRUE) / total_weight),
    by = coicop
  ]
  dt_total[, `:=`(
    series_name = paste("ES EPF 2020", coicop, "Total", sep = "."),
    year = 2020
  )]
  dt_total <- dt_total[, .(series_name, coicop, year, total_consumption)]
  setorder(dt_total, coicop)

  hbs(
    dt = dt,
    dt_total = dt_total,
    country = "ES",
    category = category_name,
    categories = categories,
    level = 3
  )
}

add_parent_coicop_rows <- function(hbs_obj) {
  dt <- copy(hbs_obj$dt)
  dt_total <- copy(hbs_obj$dt_total)

  parent_lengths <- function(code) {
    if (nchar(code) <= 2L) {
      integer()
    } else {
      seq.int(2L, nchar(code) - 1L)
    }
  }

  dt_parents <- rbindlist(lapply(sort(unique(dt$coicop)), function(code) {
    lengths <- parent_lengths(code)
    if (length(lengths) == 0L) {
      return(NULL)
    }
    rows <- dt[coicop == code]
    rbindlist(lapply(lengths, function(last) {
      out <- copy(rows)
      out[, coicop := substr(code, 1L, last)]
      out
    }), use.names = TRUE)
  }), use.names = TRUE, fill = TRUE)

  if (nrow(dt_parents) > 0L) {
    dt_parents <- dt_parents[
      ,
      .(
        series_name = paste("ES EPF 2020 parent", unique(coicop), unique(category), sep = "."),
        consumption = sum(consumption, na.rm = TRUE)
      ),
      by = .(coicop, year, category)
    ]
    dt <- rbindlist(
      list(dt, dt_parents[!dt, on = .(coicop, year, category)]),
      use.names = TRUE,
      fill = TRUE
    )
  }

  total_parents <- rbindlist(lapply(sort(unique(dt_total$coicop)), function(code) {
    lengths <- parent_lengths(code)
    if (length(lengths) == 0L) {
      return(NULL)
    }
    rows <- dt_total[coicop == code]
    rbindlist(lapply(lengths, function(last) {
      out <- copy(rows)
      out[, coicop := substr(code, 1L, last)]
      out
    }), use.names = TRUE)
  }), use.names = TRUE, fill = TRUE)

  if (nrow(total_parents) > 0L) {
    total_parents <- total_parents[
      ,
      .(
        series_name = paste("ES EPF 2020 parent", unique(coicop), "Total", sep = "."),
        total_consumption = sum(total_consumption, na.rm = TRUE)
      ),
      by = .(coicop, year)
    ]
    dt_total <- rbindlist(
      list(dt_total, total_parents[!dt_total, on = .(coicop, year)]),
      use.names = TRUE,
      fill = TRUE
    )
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

objects <- list(
  income = add_parent_coicop_rows(make_hbs("income", "income_group", paste0("Decile ", 1:10))),
  age = add_parent_coicop_rows(make_hbs(
    "age",
    "age_group",
    c("Less than 30 years", "From 30 to 44 years", "From 45 to 59 years", "60 years or over")
  )),
  urban = add_parent_coicop_rows(make_hbs("urban", "urban_group", c("Rural areas", "Towns and suburbs", "Cities")))
)

diagnostics <- rbindlist(lapply(names(objects), function(category_name) {
  obj <- objects[[category_name]]
  data.table(
    source = "INE EPF 2020 microdata, author calculations",
    category = category_name,
    n_categories = length(obj$categories),
    n_households = households[!is.na(get(paste0(category_name, "_group"))) | category_name == "income", .N],
    n_expense_rows = nrow(expenses),
    n_coicop4 = uniqueN(obj$dt$coicop),
    output_rds = file.path(out_dir, sprintf("ES_%s_epf_2020_level3.rds", category_name))
  )
}), use.names = TRUE)

for (category_name in names(objects)) {
  file_name <- sprintf("ES_%s_epf_2020_level3.rds", category_name)
  data_raw_path <- file.path(out_dir, file_name)
  ext_path <- file.path(ext_dir, file_name)
  saveRDS(objects[[category_name]], data_raw_path, compress = "xz")
  saveRDS(objects[[category_name]], ext_path, compress = "xz")
  message("Wrote ", normalizePath(data_raw_path, winslash = "/", mustWork = FALSE))
  message("Wrote ", normalizePath(ext_path, winslash = "/", mustWork = FALSE))
}

fwrite(diagnostics, file.path(out_dir, "ES_epf_2020_level3_diagnostics.csv"))
