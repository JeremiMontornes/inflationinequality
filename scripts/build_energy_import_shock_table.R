suppressPackageStartupMessages({
  library(data.table)
  if (requireNamespace("pkgload", quietly = TRUE)) {
    pkgload::load_all(".", quiet = TRUE)
  } else {
    library(inflationinequality)
  }
})

countries <- c("DE", "FR", "IT", "ES")
year <- 2023L
shock_size <- 0.10
energy_import_sectors <- c("B", "C19", "D35")

out_dir <- file.path("docs", "shock_simulations")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

format_results_for_display <- function(dt) {
  out <- copy(dt)
  display_names <- c(
    country = "Pays",
    inflation_effet = "Inflation - effet",
    inflation_effet_indirect = "Inflation - effet indirect",
    inflation_gap_revenu_effet = "Gap revenu - effet",
    inflation_gap_revenu_effet_indirect = "Gap revenu - effet indirect",
    inflation_gap_age_effet = "Gap age - effet",
    inflation_gap_age_effet_indirect = "Gap age - effet indirect",
    inflation_gap_area_effet = "Gap zone - effet",
    inflation_gap_area_effet_indirect = "Gap zone - effet indirect"
  )
  numeric_cols <- setdiff(names(out), "country")
  out[, (numeric_cols) := lapply(.SD, function(x) round(x, 3)), .SDcols = numeric_cols]
  setnames(out, names(display_names), unname(display_names))
  out[]
}

write_kable_outputs <- function(table_results, out_dir) {
  if (!requireNamespace("knitr", quietly = TRUE)) {
    warning("Package 'knitr' is not available; skipping kable outputs.", call. = FALSE)
    return(invisible(NULL))
  }

  display_table <- format_results_for_display(table_results)
  caption <- paste0(
    "Effet d'un choc de 10 % du prix de l'energie importee ",
    "(petrole, gaz, raffinage et energie), ", year
  )

  html_path <- file.path(out_dir, "energy_import_price_shock_10pct_DE_FR_IT_ES_2023.html")
  tex_path <- file.path(out_dir, "energy_import_price_shock_10pct_DE_FR_IT_ES_2023.tex")
  pdf_path <- sub("\\.tex$", ".pdf", tex_path)

  html_table <- knitr::kable(
    display_table,
    format = "html",
    caption = caption,
    align = c("l", rep("r", ncol(display_table) - 1))
  )
  if (requireNamespace("kableExtra", quietly = TRUE)) {
    html_table <- kableExtra::kable_styling(
      html_table,
      bootstrap_options = c("striped", "condensed", "responsive"),
      full_width = FALSE,
      position = "left"
    )
  }
  writeLines(
    c(
      "<!doctype html>",
      "<html>",
      "<head>",
      "<meta charset=\"utf-8\">",
      "<title>Energy import shock table</title>",
      "<style>",
      "body{font-family:Arial,sans-serif;margin:32px;color:#111827}",
      "table{border-collapse:collapse;font-size:13px}",
      "caption{caption-side:top;text-align:left;font-weight:700;margin-bottom:10px}",
      "th,td{border:1px solid #d1d5db;padding:6px 8px}",
      "th{background:#f3f4f6}",
      "td{text-align:right}",
      "td:first-child,th:first-child{text-align:left}",
      "</style>",
      "</head>",
      "<body>",
      as.character(html_table),
      "</body>",
      "</html>"
    ),
    html_path,
    useBytes = TRUE
  )

  latex_table <- knitr::kable(
    display_table,
    format = "latex",
    booktabs = TRUE,
    caption = caption,
    align = c("l", rep("r", ncol(display_table) - 1))
  )
  if (requireNamespace("kableExtra", quietly = TRUE)) {
    latex_table <- kableExtra::kable_styling(
      latex_table,
      latex_options = c("hold_position", "scale_down")
    )
  }
  tex_document <- c(
    "\\documentclass[11pt]{article}",
    "\\usepackage[margin=1in]{geometry}",
    "\\usepackage{booktabs}",
    "\\usepackage{float}",
    "\\usepackage{graphicx}",
    "\\usepackage[T1]{fontenc}",
    "\\usepackage[utf8]{inputenc}",
    "\\begin{document}",
    as.character(latex_table),
    "\\end{document}"
  )
  writeLines(tex_document, tex_path, useBytes = TRUE)

  pdflatex <- Sys.which("pdflatex")
  if (nzchar(pdflatex)) {
    old_wd <- getwd()
    on.exit(setwd(old_wd), add = TRUE)
    setwd(out_dir)
    tex_file <- basename(tex_path)
    system2(
      pdflatex,
      c("-interaction=nonstopmode", tex_file),
      stdout = TRUE,
      stderr = TRUE
    )
  }

  message("Wrote: ", normalizePath(html_path, winslash = "/", mustWork = FALSE))
  message("Wrote: ", normalizePath(tex_path, winslash = "/", mustWork = FALSE))
  if (file.exists(pdf_path)) {
    message("Wrote: ", normalizePath(pdf_path, winslash = "/", mustWork = FALSE))
  }

  invisible(list(html = html_path, tex = tex_path, pdf = if (file.exists(pdf_path)) pdf_path else NULL))
}

figaro_path <- Sys.getenv(
  "FIGARO_IO_RDS",
  file.path("data-raw", "figaro", "figaro_icio_2023.rds")
)

read_figaro_io_cache <- function(path) {
  if (!file.exists(path)) {
    stop(
      "Missing FIGARO IO cache: ", normalizePath(path, winslash = "/", mustWork = FALSE), "\n",
      "Expected an RDS list with either:\n",
      "  - A: square technical-coefficient matrix, and nodes: node/country/sector; or\n",
      "  - Z: intermediate-use matrix, output: gross output vector, and nodes.\n",
      "You can also set FIGARO_IO_RDS to another cache path. The source should be ",
      "Eurostat FIGARO 2025 edition, year 2023, industry-by-industry.",
      call. = FALSE
    )
  }

  x <- readRDS(path)
  if (!is.null(x$A) && !is.null(x$nodes)) {
    A <- as.matrix(x$A)
    nodes <- as.data.table(x$nodes)
  } else if (!is.null(x$Z) && !is.null(x$output) && !is.null(x$nodes)) {
    output <- as.numeric(x$output)
    Z <- as.matrix(x$Z)
    A <- sweep(Z, 2, output, "/")
    A[!is.finite(A)] <- 0
    nodes <- as.data.table(x$nodes)
  } else {
    stop("FIGARO cache must contain either A + nodes or Z + output + nodes.", call. = FALSE)
  }

  required_nodes <- c("node", "country", "sector")
  if (!all(required_nodes %in% names(nodes))) {
    stop("FIGARO nodes must contain columns: node, country, sector.", call. = FALSE)
  }

  list(A = A, nodes = nodes)
}

bridge_to_coicop_level <- function(bridge, level = 2L) {
  bridge <- as.data.table(bridge)
  bridge[, coicop := substr(coicop, 1, pmin(nchar(coicop), level + 1L))]
  bridge <- bridge[, .(share = sum(share, na.rm = TRUE)), by = .(country, coicop, sector)]
  bridge[, share := share / sum(share), by = .(country, coicop)]
  bridge[]
}

make_total_weights <- function(country, year_i, level = 2L) {
  w <- load_index_weights(country, level = level, start_year = year_i, end_year = year_i)
  dt <- as.data.table(w$dt)
  dt <- dt[year == year_i, .(
    country = country,
    coicop,
    weight = weight
  )]
  dt[, weight := weight / sum(weight, na.rm = TRUE)]
  dt
}

make_group_weights <- function(country, category_i, year_i, level = 2L) {
  w <- calculate_weights(
    country = country,
    category = category_i,
    level = level,
    start_year = year_i,
    end_year = year_i,
    france_insee_income_groups = "quintile"
  )
  dt <- as.data.table(w$dt)
  dt <- dt[weight_year == year_i, .(
    country = country,
    coicop,
    category,
    weight = weighted_consumption / 100
  )]
  dt[, weight := weight / sum(weight, na.rm = TRUE), by = .(country, category)]
  dt
}

message("Reading FIGARO IO cache: ", normalizePath(figaro_path, winslash = "/", mustWork = FALSE))
io <- read_figaro_io_cache(figaro_path)

message("Loading Cai-Vandyck COICOP-CPA bridge")
bridge <- load_cai_vandyck_bridge(countries = countries)
bridge <- bridge_to_coicop_level(bridge, level = 2L)

message("Loading country and household consumption weights")
total_weights <- rbindlist(lapply(countries, make_total_weights, year_i = year), use.names = TRUE)
income_weights <- rbindlist(lapply(countries, make_group_weights, category_i = "income", year_i = year), use.names = TRUE)
age_weights <- rbindlist(lapply(countries, make_group_weights, category_i = "age", year_i = year), use.names = TRUE)
area_weights <- rbindlist(lapply(countries, make_group_weights, category_i = "urban", year_i = year), use.names = TRUE)

message("Simulating imported petroleum/gas/energy shock")
raw_results <- simulate_shock(
  A = io$A,
  nodes = io$nodes,
  shock = shock_size,
  shock_sectors = energy_import_sectors,
  shock_scope = "imported",
  bridge = bridge,
  countries = countries,
  total_weights = total_weights,
  group_weights = list(
    inflation_gap_revenu = income_weights,
    inflation_gap_age = age_weights,
    inflation_gap_area = area_weights
  )
)

table_results <- raw_results[
  ,
  .(
    country,
    inflation_effet = delta_p,
    inflation_effet_indirect = delta_p_indirect,
    inflation_gap_revenu_effet = inflation_gap_revenu,
    inflation_gap_revenu_effet_indirect = inflation_gap_revenu_indirect,
    inflation_gap_age_effet = inflation_gap_age,
    inflation_gap_age_effet_indirect = inflation_gap_age_indirect,
    inflation_gap_area_effet = inflation_gap_area,
    inflation_gap_area_effet_indirect = inflation_gap_area_indirect
  )
]

out_csv <- file.path(out_dir, "energy_import_price_shock_10pct_DE_FR_IT_ES_2023.csv")
fwrite(table_results, out_csv)
write_kable_outputs(table_results, out_dir)

message("Wrote: ", normalizePath(out_csv, winslash = "/", mustWork = FALSE))
print(table_results)
