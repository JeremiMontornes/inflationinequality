portugal_idef_2015_hbs_level3_file_name <- function(category) {
  paste0("PT_", category, "_idef_2015_level3.rds")
}

portugal_idef_2015_hbs_level3_path <- function(category) {
  file_name <- portugal_idef_2015_hbs_level3_file_name(category)
  path <- system.file("extdata", file_name, package = "inflationinequality", mustWork = FALSE)

  if (!nzchar(path)) {
    candidates <- c(
      file.path("inst", "extdata", file_name),
      file.path("data-raw", "portugal_idef", file_name)
    )
    candidates <- candidates[file.exists(candidates)]
    path <- if (length(candidates) > 0L) candidates[[1L]] else file.path("inst", "extdata", file_name)
  }

  path
}

use_portugal_idef_2015_level3_hbs <- function(country, category, level, custom_hbs) {
  identical(toupper(country), "PT") &&
    isTRUE(category %in% c("income", "age", "urban")) &&
    identical(as.numeric(level), 3) &&
    (
      (is.null(custom_hbs) && file.exists(portugal_idef_2015_hbs_level3_path(category))) ||
        is_portugal_idef_2015_level3_hbs(custom_hbs, category)
    )
}

is_portugal_idef_2015_level3_hbs <- function(hbs_obj, category = NULL) {
  if (is.null(hbs_obj) || !inherits(hbs_obj, "hbs")) {
    return(FALSE)
  }

  series_names <- if (!is.null(hbs_obj$dt) && "series_name" %in% names(hbs_obj$dt)) {
    hbs_obj$dt$series_name
  } else {
    character()
  }

  identical(toupper(hbs_obj$country %||local% ""), "PT") &&
    identical(as.numeric(hbs_obj$level %||local% NA_real_), 3) &&
    (is.null(category) || identical(hbs_obj$category, category)) &&
    length(series_names) > 0L &&
    all(startsWith(series_names, "PT IDEF 2015/2016."))
}

load_portugal_idef_2015_hbs_level3 <- function(category = c("income", "age", "urban")) {
  category <- match.arg(category)
  path <- portugal_idef_2015_hbs_level3_path(category)

  if (!file.exists(path)) {
    stop(
      "Portugal IDEF 2015/2016 level-3 ", category, " HBS file could not be found. ",
      "Run scripts/build_pt_idef_2015_hbs.R, or provide custom_hbs explicitly.",
      call. = FALSE
    )
  }

  readRDS(path)
}
