# Exclusions use the target HBS/COICOP nomenclature, after price recoding.
normalize_exclude_coicop <- function(exclude_coicop, level = NULL) {
  if (is.null(exclude_coicop)) return(character())
  if (!is.character(exclude_coicop) || anyNA(exclude_coicop) ||
      any(!grepl("^[0-9]{2,6}$", exclude_coicop))) {
    stop("'exclude_coicop' must be character COICOP prefixes, e.g. c('041', '042').")
  }
  if (!is.null(level) && any(nchar(exclude_coicop) > level + 1L)) {
    stop("'exclude_coicop' is finer than the requested COICOP level; use a finer level.")
  }
  sort(unique(exclude_coicop))
}

coicop_is_excluded <- function(codes, exclude_coicop) {
  excluded <- rep(FALSE, length(codes))
  for (prefix in exclude_coicop) excluded <- excluded | startsWith(codes, prefix)
  excluded[is.na(excluded)] <- FALSE
  excluded
}

exclude_coicop_rows <- function(dt, exclude_coicop) {
  out <- data.table::copy(dt)
  out[!coicop_is_excluded(out$coicop, exclude_coicop)]
}

exclude_hbs_coicop <- function(hbs, exclude_coicop) {
  out <- hbs
  out$dt <- exclude_coicop_rows(hbs$dt, exclude_coicop)
  out$dt_total <- exclude_coicop_rows(hbs$dt_total, exclude_coicop)
  if (!nrow(out$dt)) stop("'exclude_coicop' removes all HBS products.")
  out
}
