spain_epf_2020_hbs_level3_file_name <- function(category) {
  paste0("ES_", category, "_epf_2020_level3.rds")
}

spain_epf_2020_hbs_level3_path <- function(category) {
  file_name <- spain_epf_2020_hbs_level3_file_name(category)
  path <- system.file("extdata", file_name, package = "inflationinequality", mustWork = FALSE)

  if (!nzchar(path)) {
    candidates <- c(
      file.path("inst", "extdata", file_name),
      file.path("data-raw", "spain_epf", file_name)
    )
    candidates <- candidates[file.exists(candidates)]
    path <- if (length(candidates) > 0L) candidates[[1L]] else file.path("inst", "extdata", file_name)
  }

  path
}

use_spain_epf_2020_level3_hbs <- function(country, category, level, custom_hbs) {
  is.null(custom_hbs) &&
    identical(toupper(country), "ES") &&
    isTRUE(category %in% c("income", "age", "urban")) &&
    identical(as.numeric(level), 3) &&
    file.exists(spain_epf_2020_hbs_level3_path(category))
}

load_spain_epf_2020_hbs_level3 <- function(category = c("income", "age", "urban")) {
  category <- match.arg(category)
  path <- spain_epf_2020_hbs_level3_path(category)

  if (!file.exists(path)) {
    stop(
      "Spain EPF 2020 level-3 ", category, " HBS file could not be found. ",
      "Run scripts/build_es_epf_2020_hbs.R after downloading INE datos_2020.zip, ",
      "or provide custom_hbs explicitly.",
      call. = FALSE
    )
  }

  readRDS(path)
}
