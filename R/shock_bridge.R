#' Load Cai-Vandyck COICOP-to-CPA bridge matrices
#'
#' @description
#' Loads the country-specific bridge matrices from Cai and Vandyck (2020),
#' "Bridging between economy-wide activity and household-level consumption
#' data: Matrices for European countries". The workbook stores CPA products in
#' rows and 35 COICOP consumption categories in columns. This function converts
#' each country sheet to long form and normalises CPA values within each
#' country-COICOP column.
#'
#' The returned table can be passed directly to [simulate_shock()] as `bridge`.
#'
#' @param path Local path to the Cai-Vandyck workbook. If `NULL`, `url` is
#'   downloaded to `cache_dir`.
#' @param url URL of the workbook to download when `path` is `NULL`.
#' @param cache_dir directory used to cache the downloaded workbook.
#' @param countries optional character vector of country sheets to keep.
#' @param figaro_64 if `TRUE`, recode the few Cai-Vandyck CPA labels that differ
#'   from the FIGARO 64-sector labels used elsewhere in the package
#'   (`D -> D35`, `L68 -> L`, `O -> O84`, `P -> P85`).
#' @param keep_value if `TRUE`, keep the original bridge value before
#'   normalisation.
#'
#' @returns A data.table with columns `country`, `coicop`, `sector`, and
#'   `share`; `sector` is the CPA code without the `CPA_` prefix.
#'
#' @references
#' Cai, M. and Vandyck, T. (2020). Bridging between economy-wide activity and
#' household-level consumption data: Matrices for European countries. Data in
#' Brief, 30, 105395. doi:10.1016/j.dib.2020.105395.
#'
#' @examples
#' \dontrun{
#' bridge <- load_cai_vandyck_bridge(countries = c("FR", "DE"))
#' }
#'
#' @export
load_cai_vandyck_bridge <- function(
    path = NULL,
    url = "https://ars.els-cdn.com/content/image/1-s2.0-S2352340920302894-mmc1.xlsx",
    cache_dir = tools::R_user_dir("inflationinequality", "cache"),
    countries = NULL,
    figaro_64 = TRUE,
    keep_value = FALSE) {
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("Package 'readxl' is required to read the Cai-Vandyck workbook.", call. = FALSE)
  }

  if (is.null(path)) {
    dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
    path <- file.path(cache_dir, basename(url))
    if (!file.exists(path)) {
      utils::download.file(url, path, mode = "wb", quiet = TRUE)
    }
  }

  sheets <- readxl::excel_sheets(path)
  metadata_sheets <- c("geo", "cpa", "coicop", "dataByCOICOP", "dataByCPA", "correspondences")
  country_sheets <- setdiff(sheets, metadata_sheets)
  if (!is.null(countries)) {
    countries <- toupper(countries)
    country_sheets <- intersect(country_sheets, countries)
    missing_countries <- setdiff(countries, country_sheets)
    if (length(missing_countries) > 0) {
      stop(
        "No Cai-Vandyck bridge sheet found for: ",
        paste(missing_countries, collapse = ", "),
        call. = FALSE
      )
    }
  }

  out <- data.table::rbindlist(lapply(country_sheets, function(country) {
    dt <- data.table::as.data.table(readxl::read_excel(path, sheet = country))
    sector_col <- names(dt)[1]
    data.table::setnames(dt, sector_col, "sector")
    dt[, sector := sub("^CPA_", "", as.character(sector))]
    if (isTRUE(figaro_64)) {
      dt[, sector := recode_cai_vandyck_sector_to_figaro64(sector)]
    }
    long <- data.table::melt(
      dt,
      id.vars = "sector",
      variable.name = "coicop",
      value.name = "value"
    )
    long[, `:=`(
      country = country,
      coicop = normalize_coicop_code(coicop),
      value = as.numeric(value)
    )]
    long <- long[!is.na(sector) & !is.na(coicop) & is.finite(value) & value > 0]
    long[, share := value / sum(value), by = .(country, coicop)]
    long
  }), use.names = TRUE, fill = TRUE)

  if (!keep_value) {
    out[, value := NULL]
  }

  data.table::setcolorder(out, intersect(c("country", "coicop", "sector", "share", "value"), names(out)))
  data.table::setorder(out, country, coicop, sector)
  out[]
}

recode_cai_vandyck_sector_to_figaro64 <- function(sector) {
  data.table::fcase(
    sector == "D", "D35",
    sector == "L68", "L",
    sector == "O", "O84",
    sector == "P", "P85",
    default = sector
  )
}
