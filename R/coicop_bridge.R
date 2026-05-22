#' Build the COICOP bridge used to match HICP and HBS data
#'
#' @description
#' `build_coicop_bridge()` shows how HICP COICOP weight codes are matched to
#' HBS COICOP consumption codes before [calculate_weights()] computes household
#' category weights.
#'
#' @inheritParams calculate_weights
#'
#' @returns A `data.table` with one row per HICP COICOP, category, and HICP
#' weight year. It includes the HICP code, the HBS code used by
#' `calculate_weights()`, the selected HBS wave, and the mapping status.
#'
#' @examples
#' \dontrun{
#' bridge <- build_coicop_bridge("FR", "income", start_year = 2019)
#' write_coicop_bridge_html(bridge, "coicop_bridge.html")
#' }
#'
#' @export
build_coicop_bridge <- function(country = NULL, category = NULL, level = 2,
                                start_year = NULL, end_year = NULL,
                                custom_index_weights = NULL,
                                custom_hbs = NULL,
                                interpolated_hbs = FALSE,
                                specific_hbs_year = NULL) {
  if (!is.null(country)) {
    country <- toupper(country)
  }

  inputs <- load_coicop_bridge_inputs(
    country = country,
    category = category,
    level = level,
    start_year = start_year,
    end_year = end_year,
    custom_index_weights = custom_index_weights,
    custom_hbs = custom_hbs,
    interpolated_hbs = interpolated_hbs,
    specific_hbs_year = specific_hbs_year
  )

  index_weights <- inputs$index_weights
  hbs <- inputs$hbs
  specific_hbs_year <- inputs$specific_hbs_year
  country_value <- country %||local% index_weights$country %||local% hbs$country
  category_type_value <- category %||local% hbs$category

  weight_dt <- data.table::copy(index_weights$dt)
  if ("year" %in% names(weight_dt)) {
    data.table::setnames(weight_dt, "year", "weight_year")
  } else if (!"weight_year" %in% names(weight_dt)) {
    stop("Index weights must contain either 'year' or 'weight_year'.")
  }

  hbs_dt <- data.table::copy(hbs$dt)
  hbs_total <- data.table::copy(hbs$dt_total)
  hbs_dt <- hbs_dt[hbs_total, on = .(coicop, year)]
  if (!is.null(specific_hbs_year)) {
    hbs_dt <- hbs_dt[year == specific_hbs_year]
  }

  weight_coicops <- weight_dt[nchar(coicop) == index_weights$level + 1, unique(coicop)]
  hbs_coicops <- hbs_dt[nchar(coicop) == hbs$level + 1, unique(coicop)]
  missing_coicops <- setdiff(weight_coicops, hbs_coicops)
  higher_coicops <- unique(substr(missing_coicops, 1, index_weights$level))

  mapping <- data.table::data.table(hicp_coicop = weight_coicops)
  mapping[, hbs_coicop := data.table::fifelse(
    substr(hicp_coicop, 1, index_weights$level) %in% higher_coicops,
    substr(hicp_coicop, 1, index_weights$level),
    hicp_coicop
  )]
  mapping[, mapping_status := data.table::fifelse(
    hicp_coicop == hbs_coicop,
    "exact",
    "rolled_up_to_higher_level"
  )]

  hbs_available <- unique(hbs_dt$coicop)
  mapping[, hbs_code_available := hbs_coicop %in% hbs_available]

  bridge_raw <- mapping[
    weight_dt,
    on = .(hicp_coicop = coicop),
    allow.cartesian = TRUE,
    nomatch = 0
  ]

  hbs_for_join <- hbs_dt[
    ,
    .(
      hbs_coicop = coicop,
      hbs_year = year,
      category,
      consumption,
      total_consumption
    )
  ]
  bridge_raw <- hbs_for_join[
    bridge_raw,
    on = .(hbs_coicop),
    allow.cartesian = TRUE,
    nomatch = 0
  ]

  selected <- bridge_raw[
    ,
    {
      temp <- .SD
      if (.N > 0 && any(hbs_year <= weight_year)) {
        temp <- temp[hbs_year <= weight_year]
        temp[, .SD[which.max(hbs_year)]]
      } else {
        temp[, .SD[which.min(hbs_year)]]
      }
    },
    by = .(hicp_coicop, hbs_coicop, category, weight_year)
  ]

  selected[
    ,
    .(
      country = country_value,
      category_type = category_type_value,
      category,
      weight_year,
      hbs_year,
      hicp_coicop,
      hbs_coicop,
      mapping_status,
      hicp_weight = weight,
      hbs_consumption = consumption,
      hbs_total_consumption = total_consumption,
      hbs_code_available
    )
  ][order(weight_year, hicp_coicop, category)]
}

#' Write a COICOP bridge table to HTML
#'
#' @param bridge A `data.table` returned by [build_coicop_bridge()].
#' @param file Output HTML file path.
#' @param title HTML page title.
#' @param compact If `TRUE`, write one row per unique HICP-to-HBS COICOP
#'   mapping. If `FALSE`, keep the audit table by year.
#' @param latest_weight_year_only If `TRUE`, keep only the latest HICP weight
#'   year available in `bridge`.
#' @param include_category If `TRUE`, keep household-category columns in the
#'   HTML table.
#'
#' @returns The output file path, invisibly.
#'
#' @export
write_coicop_bridge_html <- function(bridge, file, title = "COICOP HICP-HBS bridge",
                                     compact = TRUE,
                                     latest_weight_year_only = FALSE,
                                     include_category = FALSE) {
  if (!data.table::is.data.table(bridge)) {
    bridge <- data.table::as.data.table(bridge)
  }

  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  bridge <- data.table::copy(bridge)
  if (latest_weight_year_only && "weight_year" %in% names(bridge)) {
    bridge <- bridge[weight_year == max(weight_year, na.rm = TRUE)]
  }

  display <- if (compact) {
    bridge[
      ,
      .(
        country = paste(sort(unique(country)), collapse = ", "),
        hicp_coicop,
        hbs_coicop,
        mapping_status,
        first_weight_year = min(weight_year, na.rm = TRUE),
        last_weight_year = max(weight_year, na.rm = TRUE),
        hbs_years = paste(sort(unique(hbs_year)), collapse = ", "),
        hbs_code_available = all(hbs_code_available)
      ),
      by = .(hicp_coicop, hbs_coicop, mapping_status)
    ][order(hicp_coicop)]
  } else {
    if (!include_category) {
      bridge[, c("category", "category_type") := NULL]
    }
    unique(bridge)
  }
  display <- display[
    ,
    lapply(.SD, function(x) {
      if (is.numeric(x)) {
        round(x, 4)
      } else {
        x
      }
    })
  ]

  html <- paste0(
    "<!doctype html>\n<html lang=\"en\">\n<head>\n<meta charset=\"utf-8\">\n",
    "<title>", html_escape(title), "</title>\n",
    "<style>",
    "body{font-family:Arial,sans-serif;margin:24px;color:#222}",
    "table{border-collapse:collapse;width:100%;font-size:12px}",
    "th,td{border:1px solid #ddd;padding:6px 8px;text-align:left}",
    "th{position:sticky;top:0;background:#f3f3f3}",
    "tr:nth-child(even){background:#fafafa}",
    ".meta{color:#555;margin-bottom:16px}",
    "</style>\n</head>\n<body>\n",
    "<h1>", html_escape(title), "</h1>\n",
    "<p class=\"meta\">Rows: ", nrow(display), ". Grey-level check table generated from the same HICP-HBS matching rule used by calculate_weights().</p>\n",
    data_table_to_html(display),
    "\n</body>\n</html>\n"
  )

  writeLines(html, file, useBytes = TRUE)
  invisible(file)
}

load_coicop_bridge_inputs <- function(country, category, level, start_year, end_year,
                                      custom_index_weights, custom_hbs,
                                      interpolated_hbs, specific_hbs_year) {
  index_weights <- if (is.null(custom_index_weights)) {
    if (is.null(country)) {
      stop("Either 'country' or 'custom_index_weights' must be provided.")
    }
    load_index_weights(country, level = level, start_year = start_year, end_year = end_year)
  } else {
    custom_index_weights
  }

  hbs <- if (is.null(custom_hbs)) {
    if (is.null(country) || is.null(category)) {
      stop("Either both 'country' and 'category', or 'custom_hbs' must be provided.")
    }
    load_hbs(country, category, level = level)
  } else {
    custom_hbs
  }

  if (is.null(custom_hbs) &&
      is.null(specific_hbs_year) &&
      identical(country, "IT") &&
      identical(category, "income") &&
      2010 %in% hbs$dt[, unique(year)]) {
    specific_hbs_year <- 2010
  }

  if (interpolated_hbs) {
    hbs <- interpolate_hbs(hbs)
  }

  list(index_weights = index_weights, hbs = hbs, specific_hbs_year = specific_hbs_year)
}

data_table_to_html <- function(dt) {
  header <- paste0("<tr>", paste0("<th>", html_escape(names(dt)), "</th>", collapse = ""), "</tr>")
  rows <- apply(dt, 1, function(row) {
    paste0("<tr>", paste0("<td>", html_escape(row), "</td>", collapse = ""), "</tr>")
  })
  paste0("<table>\n<thead>", header, "</thead>\n<tbody>\n", paste(rows, collapse = "\n"), "\n</tbody>\n</table>")
}

html_escape <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  x
}

`%||local%` <- function(x, y) {
  if (is.null(x)) y else x
}
