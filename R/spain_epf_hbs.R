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

  add_spain_epf_parent_coicop_rows(readRDS(path))
}

add_spain_epf_parent_coicop_rows <- function(hbs_obj) {
  dt <- data.table::copy(hbs_obj$dt)
  dt_total <- data.table::copy(hbs_obj$dt_total)

  parent_lengths <- function(code) {
    if (nchar(code) <= 2L) {
      integer()
    } else {
      seq.int(2L, nchar(code) - 1L)
    }
  }

  dt_parents <- data.table::rbindlist(lapply(sort(unique(dt$coicop)), function(code) {
    lengths <- parent_lengths(code)
    if (length(lengths) == 0L) {
      return(NULL)
    }
    rows <- dt[coicop == code]
    data.table::rbindlist(lapply(lengths, function(last) {
      out <- data.table::copy(rows)
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
    dt <- data.table::rbindlist(
      list(dt, dt_parents[!dt, on = .(coicop, year, category)]),
      use.names = TRUE,
      fill = TRUE
    )
  }

  total_parents <- data.table::rbindlist(lapply(sort(unique(dt_total$coicop)), function(code) {
    lengths <- parent_lengths(code)
    if (length(lengths) == 0L) {
      return(NULL)
    }
    rows <- dt_total[coicop == code]
    data.table::rbindlist(lapply(lengths, function(last) {
      out <- data.table::copy(rows)
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
    dt_total <- data.table::rbindlist(
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
