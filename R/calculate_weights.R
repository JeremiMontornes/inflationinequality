#' Calculates combined weights from CPI and HBS data
#'
#' @description
#' `calculate_weights()` combines annual Consumer Price Index (CPI) weight data with Household Budget Survey (HBS) weight data to create a comprehensive dataset for economic analysis.
#'
#' @details
#' The function performs the following key operations:
#' 1. Temporal matching: CPI weights are matched with the most recent prior HBS wave. If no prior wave exists, the earliest available HBS wave is used.
#' 2. COICOP code alignment: A left join is performed on COICOP codes, ensuring all CPI COICOP codes are represented in the final dataset.
#' 3. Weight normalization: Final weights are normalized to sum to 100% for each category and year.
#'
#' Merging logic:
#' * For each CPI weight year, the function selects HBS data from the most recent prior wave.
#' * Example: CPI weights from 2015-2019 are merged with the 2015 HBS wave; 2020 CPI weights with the 2020 HBS wave.
#' * For countries with limited HBS data (e.g., France's earliest wave is 2005), all earlier CPI data points use the earliest available HBS wave.
#' * For Italy income groups, if the Eurostat 2010 HBS wave is available, it is used for all CPI weight years by default.
#'
#' Data handling:
#' * COICOP codes present in CPI data but absent in HBS data are included with minimal consumption values (1e-6) to avoid data loss.
#' * Zero values in both CPI and HBS data are replaced with 1e-6 to prevent division by zero errors.
#'
#' Weight calculation steps:
#' 1. Multiply CPI weights with HBS consumption data.
#' 2. Normalize by dividing by total consumption for each COICOP code and year.
#' 3. Scale weights to sum to 100% for each category and year.
#'
#' @inheritParams load_hbs
#' @param country 2-digit country code (see ISO 3166-1 alpha-2), only one
#'   country at a time is accepted. The parameter is optional since we can load
#'   `custom_index_weights` and `custom_hbs`, but if both of those arguments are
#'   missing, then the `country` parameter is mandatory.
#' @param category HBS data by category: `"income"`, `"age"`, `"urban"`. The
#'   parameter is optional since one can load `custom_hbs`, but it is mandatory
#'   if that argument is missing.
#' @param custom_index_weights an object of class `"index_weights"`.
#' @param custom_hbs an object of class `"hbs"`.
#' @param interpolated_hbs flag if you want to interpolate HBS weights
#' @param specific_hbs_year year of selected HBS wave. It's recommended to
#'   download HBS data first to see what HBS years are available.
#' @param france_insee_income_groups grouping used for the bundled France
#'   INSEE level-3 income HBS data. `"decile"` keeps the original INSEE decile
#'   means. `"quintile"` averages adjacent decile means (D1-D2, D3-D4, ...)
#'   because the source values are mean expenditures for equal-population
#'   decile groups, not income-threshold bounds. This option only affects
#'   `category = "income"`; France `age` and `urban` level-3 HBS data keep their
#'   original national groups.
#' @param weighting_method weighting formula. `"relative_expenditure"` keeps the
#'   historical package formula. `"ras"` applies iterative proportional fitting.
#'   `"additive_qp"` preserves absolute HBS differences between groups and, when
#'   necessary, projects the resulting weights onto the non-negative weights
#'   satisfying the group-basket and HICP-item margins. At COICOP level 2, all
#'   weighting methods combine HBS actual rents (`04.1`) and imputed rents of
#'   owner-occupiers (`04.2`) to distribute the HICP actual-rents weight
#'   (`04.1`). This housing bridge is applied for every country when both HBS
#'   components are available; HICP weights remain unchanged.
#'
#' @returns An object of class `"weights"` is a list containing the following
#'   components:
#' - `dt`: a `data.table` object (see below).
#' - `dt_coverage`: a `data.table` object (see below).
#' - `dt_coicop_bridge`: a `data.table` showing the HICP-to-HBS COICOP mapping
#' used in the calculation.
#' - `country`: 2-digit country code (see ISO 3166-1 alpha-2).
#' - `category`: HBS category: `"income"`, `"age"`, or `"urban"`.
#' - `categories`: (Ordered) vector of category types, from lowest to highest.
#' - `level`: COICOP level.
#' - `start_year`: first year of data.
#' - `last_year`: last year of data.
#'
#' The component `dt` has the following columns:
#' - `series_name`: identifier for the data series.
#' - `coicop`: COICOP code.
#' - `year`: year of the HBS data.
#' - `category`: HBS category (e.g., "First quintile").
#' - `weighted_consumption`: calculated weight (normalized to sum to 100 within
#' each category and year).
#' - `weight_year`: year of the CPI weight data.
#'
#' The component `dt_coverage` has the following columns:
#' \describe{
#'   \item{weight_year}{year}
#'   \item{weight_sum_avg}{total weight coverage of price index in percentage points}
#' }
#'
#' @examples
#' \dontrun{
#' # Calculate weights for France, income category, COICOP level 2, from 2010 to 2020
#' france_weights <- calculate_weights("FR", "income", level = 2, start_year = 2010, end_year = 2020)
#'
#' # Check if weights sum to 100 for a specific category and year
#' france_weights$dt[category == "First quintile" & weight_year == 2015, sum(weighted_consumption)]
#'
#' # Access the data.table component
#' dt_weights <- france_weights$dt
#' }
#'
#' @seealso [load_index_weights()], [load_hbs()], [index_weights()], [hbs()]
#'
#' @importFrom data.table :=
#' @export
calculate_weights <- function(country = NULL, category = NULL, level = 2,
                              start_year = NULL, end_year = NULL,
                              custom_index_weights = NULL,
                              custom_hbs = NULL,
                              interpolated_hbs = FALSE,
                              specific_hbs_year = NULL,
                              france_insee_income_groups = c("decile", "quintile"),
                              weighting_method = c("relative_expenditure", "ras", "additive_qp")) {
  if (!is.null(country)) {
    country <- toupper(country)
  }
  france_insee_income_groups <- match.arg(france_insee_income_groups)
  weighting_method <- match.arg(weighting_method)

  # Load index weights
  index_weights <- if (is.null(custom_index_weights)) {
    if (is.null(country)) {
      stop("Either 'country' or 'custom_index_weights' must be provided.")
    }
    load_index_weights(
      country, level = level,
      start_year = start_year, end_year = end_year)
  } else {
    # Check if date range is sufficient
    if (!is.null(start_year)) {
      if (start_year < custom_index_weights$start_year) {
        stop(paste0("Not enough CPI weight data. Latest possible start year: ", start_year))
      }
    }

    if (!is.null(end_year)) {
      if (end_year > custom_index_weights$end_year) {
        stop(paste0("Not enough CPI weight data. Earliest possible end year: ", end_year))
      }
    }

    custom_index_weights
  }

  # Load HBS data
  hbs <- if (is.null(custom_hbs)) {
    if (is.null(country) || is.null(category)) {
      stop("Either both 'country' and 'category', or 'custom_hbs' must be provided.")
    }
    italy_hbs <- load_italy_level2_hbs_if_available(country, category, level)
    if (!is.null(italy_hbs)) {
      italy_hbs
    } else if (use_spain_epf_2020_level3_hbs(country, category, level, custom_hbs)) {
      load_spain_epf_2020_hbs_level3(category = category)
    } else if (use_france_insee_level3_hbs(country, category, level, custom_hbs)) {
      load_france_insee_hbs_level3(category = category, income_groups = france_insee_income_groups)
    } else {
      load_hbs(
        country, category,
        level = level)
    }
  } else {
    # Check if date range is sufficient
    if (!is.null(start_year)) {
      if (start_year < custom_hbs$start_year) {
        stop(paste0("Not enough HBS weight data. Latest possible start year: ", start_year))
      }
    }

    if (!is.null(end_year)) {
      if (end_year > custom_hbs$end_year) {
        stop(paste0("Not enough HBS weight data. Earliest possible end year: ", end_year))
      }
    }

    if (custom_hbs$category != category) {
      stop("Category of custom_hbs object does not match the category parameter.")
    }

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

  if (identical(as.integer(level), 2L)) {
    hbs <- combine_hbs_actual_and_imputed_rents(hbs)
  }

  dt_coicop_bridge <- build_coicop_bridge(
    country = country,
    category = category,
    level = level,
    custom_index_weights = index_weights,
    custom_hbs = hbs,
    specific_hbs_year = specific_hbs_year
  )

  dt_weighted_consumption <- if (use_spain_epf_2020_level3_hbs(country, category, level, custom_hbs)) {
    merge_spain_epf_level3_index_and_hbs(index_weights, hbs, specific_hbs_year)
  } else if (use_france_insee_level3_hbs(country, category, level, custom_hbs)) {
    merge_france_tf106_level3_index_and_hbs(index_weights, hbs, specific_hbs_year)
  } else {
    merge_index_and_hbs(index_weights, hbs, specific_hbs_year)
  }

  dt_weighted_consumption <- dt_weighted_consumption[,
    {
      # Create a temporary copy of the current subset (.SD) of the data.table
      temp <- .SD

      # Check if the current group has any rows (.N > 0)
      # and if there are any rows where 'year' is less than or equal to 'weight_year'
      if (.N > 0 && any(year <= weight_year)) {
        # If both conditions are true:
        # 1. Filter the temporary data.table to keep only rows where 'year' <= 'weight_year'
        temp <- temp[year <= weight_year]

        # 2. From the filtered data.table, select the row with the maximum 'year' value
        # This is done using '.SD[which.max(year)]', which returns a data.table with a single row
        # containing the row with the maximum 'year' value in the current group
        temp <- temp[, .SD[which.max(year)]]
      } else {
        # If either condition is false (the group is empty or no rows have 'year' <= 'weight_year'):
        # Select the row with the minimum 'year' value from the temporary data.table
        # This is done using '.SD[which.min(year)]', which returns a data.table with a single row
        # containing the row with the minimum 'year' value in the current group
        temp <- temp[, .SD[which.min(year)]]
      }

      # Return the temporary data.table 'temp' after performing the desired operations
      temp

      # The 'by' argument specifies the columns to group the data by
      # In this case, the grouping is done by 'coicop', 'category', and 'weight_year'
      # This means that the operations inside the curly braces are performed for each unique
      # combination of 'coicop', 'category', and 'weight_year'
    },
    by = .(coicop, category, weight_year)
  ]

  # Test weight is 100%
  dt_sums <- dt_weighted_consumption[, .(weight_sum = sum(weight)), by = .(weight_year, category)]
  dt_avg <- dt_sums[, .(weight_sum_avg = mean(weight_sum) * 100 / index_weights$base_total), by = .(weight_year)]

  if (identical(weighting_method, "relative_expenditure")) {
    ### Equation (1)
    dt_weighted_consumption[, hbs_multiplier := data.table::fifelse(
      consumption == 1e-6 & total_consumption == 1e-6,
      1e-6,
      consumption / total_consumption
    )]

    dt_weighted_consumption[, unnormalized_weighted_consumption := weight * hbs_multiplier]
    ###

    # Normalised weights
    dt_weighted_consumption[, weighted_consumption := unnormalized_weighted_consumption * 100 / sum(unnormalized_weighted_consumption), by = .(weight_year, category)]
  } else if (identical(weighting_method, "ras")) {
    dt_weighted_consumption <- apply_ras_group_weights(
      dt_weighted_consumption,
      hbs_category = hbs$category,
      categories = hbs$categories,
      country = country
    )
  } else {
    dt_weighted_consumption <- apply_additive_qp_group_weights(
      dt_weighted_consumption,
      hbs_category = hbs$category,
      categories = hbs$categories,
      country = country
    )
  }

  # Remove intermediate columns to reduce memory usage
  intermediate_cols <- intersect(
    c(
      "weight", "consumption", "hbs_multiplier", "total_consumption",
      "unnormalized_weighted_consumption", "seed"
    ),
    names(dt_weighted_consumption)
  )
  if (length(intermediate_cols) > 0L) {
    dt_weighted_consumption[, (intermediate_cols) := NULL]
  }

  # Stop if there are abnormally large weights
  abnormal_weighted_consumption <-
    dt_weighted_consumption[weighted_consumption >= 90, ]

  if (nrow(abnormal_weighted_consumption) > 0) {
    stop(
      "There are weights that are anormally large (>=90%):\n",
      paste(capture.output(head(abnormal_weighted_consumption[, .(coicop, category, weight_year, year, weighted_consumption)], n = 10)),
            collapse = "\n"))
  }

  # Usually, weights don't go above 20% but it doesn't mean there's an error.
  very_large_weighted_consumption <-
    dt_weighted_consumption[weighted_consumption >= 20, ]

  if (nrow(very_large_weighted_consumption) > 0) {
    message(
      "There are weights that are very large (>=20%):\n",
      paste(capture.output(head(very_large_weighted_consumption[, .(coicop, category, weight_year, year, weighted_consumption)], n = 10)),
            collapse = "\n"))
  }

  return(structure(list(dt = dt_weighted_consumption,
                        dt_coverage = dt_avg,
                        dt_coicop_bridge = dt_coicop_bridge,
                        country = country,
                        category = category,
                        categories = hbs$categories,
                        weighting_method = weighting_method,
                        level = level,
                        start_year = min(dt_weighted_consumption$weight_year),
                        end_year = max(dt_weighted_consumption$weight_year)),
                   class = "weights"))
}

combine_hbs_actual_and_imputed_rents <- function(hbs) {
  housing_codes <- c("041", "042")
  if (!all(housing_codes %in% unique(hbs$dt$coicop))) {
    return(hbs)
  }

  out <- hbs
  dt <- data.table::copy(hbs$dt)
  dt_total <- data.table::copy(hbs$dt_total)

  housing <- dt[
    coicop %in% housing_codes,
    .(
      series_name = paste(sort(unique(stats::na.omit(series_name))), collapse = " + "),
      consumption = sum(consumption, na.rm = TRUE)
    ),
    by = .(year, category)
  ]
  housing[, coicop := "041"]
  data.table::setcolorder(
    housing,
    intersect(names(dt), c("series_name", "coicop", "year", "category", "consumption"))
  )

  housing_total <- dt_total[
    coicop %in% housing_codes,
    .(
      series_name = if ("series_name" %in% names(dt_total)) {
        paste(sort(unique(stats::na.omit(series_name))), collapse = " + ")
      } else {
        NA_character_
      },
      total_consumption = sum(total_consumption, na.rm = TRUE)
    ),
    by = year
  ]
  housing_total[, coicop := "041"]
  if (!"series_name" %in% names(dt_total)) {
    housing_total[, series_name := NULL]
  }
  data.table::setcolorder(
    housing_total,
    intersect(names(dt_total), c("series_name", "coicop", "year", "total_consumption"))
  )

  out$dt <- data.table::rbindlist(
    list(dt[!coicop %in% housing_codes], housing),
    use.names = TRUE,
    fill = TRUE
  )
  out$dt_total <- data.table::rbindlist(
    list(dt_total[!coicop %in% housing_codes], housing_total),
    use.names = TRUE,
    fill = TRUE
  )
  data.table::setorder(out$dt, coicop, year, category)
  data.table::setorder(out$dt_total, coicop, year)
  out$combined_hbs_housing_041_042 <- TRUE
  out
}

apply_additive_qp_group_weights <- function(dt, hbs_category, categories,
                                            country = NULL,
                                            tolerance = 1e-8,
                                            max_iter = 100000L) {
  categories <- normalize_group_labels(categories)
  dt[, category := normalize_group_labels(category)]
  if (!all(categories %in% unique(dt$category))) {
    stop(
      "Cannot apply additive QP because not all HBS categories are present ",
      "in the HBS-HICP matched data.",
      call. = FALSE
    )
  }

  category_share <- ras_category_shares(
    hbs_category = hbs_category,
    country = country,
    categories = categories,
    weight_years = sort(unique(dt$weight_year))
  )
  dt <- category_share[dt, on = .(category, weight_year)]
  if (anyNA(dt$category_share)) {
    stop("Missing group consumption shares for additive calibration.", call. = FALSE)
  }

  dt <- dt[
    ,
    additive_qp_calibrate_group(
      .SD,
      categories = categories,
      tolerance = tolerance,
      max_iter = max_iter
    ),
    by = weight_year
  ]
  dt[, category_share := NULL]
  dt[]
}

additive_qp_calibrate_group <- function(dt, categories, tolerance, max_iter) {
  categories <- categories[categories %in% unique(dt$category)]
  coicops <- sort(unique(dt$coicop))
  n_group <- length(categories)
  n_item <- length(coicops)

  category_share <- dt[
    , .(category_share = category_share[1L]), by = category
  ][match(categories, category), category_share]
  category_share <- category_share / sum(category_share)
  hicp_target <- dt[
    , .(weight = weight[1L]), by = coicop
  ][match(coicops, coicop), weight]
  hicp_target <- hicp_target * 100 / sum(hicp_target)

  hbs <- matrix(0, nrow = n_group, ncol = n_item,
                dimnames = list(categories, coicops))
  for (q in seq_along(categories)) {
    values <- dt[category == categories[q]][match(coicops, coicop), consumption]
    values[!is.finite(values) | values < 0] <- 0
    if (sum(values) <= 0) {
      hbs[q, ] <- hicp_target
    } else {
      hbs[q, ] <- values * 100 / sum(values)
    }
  }
  hbs_mean <- drop(crossprod(category_share, hbs))
  additive <- sweep(hbs, 2L, hbs_mean, "-")
  additive <- sweep(additive, 2L, hicp_target, "+")
  unsupported <- dt[
    , .(unsupported = all(total_consumption <= 1e-6)), by = coicop
  ][match(coicops, coicop), unsupported]
  # An HICP item with no HBS support carries no distributional signal: assign
  # its national HICP weight to every group before projecting the full system.
  if (any(unsupported)) {
    additive[, unsupported] <- rep(hicp_target[unsupported], each = n_group)
  }

  calibrated <- additive_qp_project(
    additive,
    category_share = category_share,
    hicp_target = hicp_target,
    tolerance = tolerance,
    max_iter = max_iter
  )

  calibrated_dt <- data.table::as.data.table(as.table(calibrated))
  data.table::setnames(
    calibrated_dt,
    c("category", "coicop", "weighted_consumption")
  )
  calibrated_dt[, `:=`(
    category = as.character(category),
    coicop = as.character(coicop),
    weighted_consumption = as.numeric(weighted_consumption)
  )]
  calibrated_dt[dt, on = .(category, coicop)][]
}

additive_qp_project <- function(seed, category_share, hicp_target,
                                tolerance = 1e-8, max_iter = 100000L) {
  n_group <- nrow(seed)
  n_item <- ncol(seed)
  n_variable <- n_group * n_item

  # One HICP constraint is redundant with the basket constraints and is
  # omitted so that the affine projection has full row rank.
  constraint <- matrix(0, nrow = n_group + n_item - 1L,
                       ncol = n_variable)
  for (q in seq_len(n_group)) {
    constraint[q, q + n_group * (seq_len(n_item) - 1L)] <- 1
  }
  if (n_item > 1L) {
    for (j in seq_len(n_item - 1L)) {
      constraint[n_group + j,
                 seq_len(n_group) + n_group * (j - 1L)] <- category_share
    }
  }
  target <- c(rep(100, n_group), hicp_target[seq_len(n_item - 1L)])
  gram <- tcrossprod(constraint)
  project_affine <- function(value) {
    residual <- target - drop(constraint %*% value)
    value + drop(crossprod(constraint, qr.solve(gram, residual)))
  }

  current <- as.vector(seed)
  affine_correction <- numeric(n_variable)
  nonnegative_correction <- numeric(n_variable)
  for (iter in seq_len(max_iter)) {
    affine_input <- current + affine_correction
    affine <- project_affine(affine_input)
    affine_correction <- affine_input - affine

    nonnegative_input <- affine + nonnegative_correction
    updated <- pmax(nonnegative_input, 0)
    nonnegative_correction <- nonnegative_input - updated

    margin_error <- max(abs(drop(constraint %*% updated) - target))
    if (max(abs(updated - current)) < tolerance && margin_error < tolerance) {
      out <- matrix(updated, nrow = n_group, ncol = n_item,
                    dimnames = dimnames(seed))
      if (min(out) < -tolerance ||
          max(abs(rowSums(out) - 100)) > 10 * tolerance ||
          max(abs(drop(crossprod(category_share, out)) - hicp_target)) >
            10 * tolerance) {
        stop("Additive QP calibration failed its margin checks.", call. = FALSE)
      }
      out[out < 0 & out > -tolerance] <- 0
      return(out)
    }
    current <- updated
  }
  stop("Additive QP calibration did not converge.", call. = FALSE)
}

apply_ras_group_weights <- function(dt, hbs_category, categories, country = NULL,
                                    tolerance = 1e-10, max_iter = 1000L) {
  categories <- normalize_group_labels(categories)
  dt[, category := normalize_group_labels(category)]
  n_categories <- length(categories)
  if (identical(hbs_category, "income") && !n_categories %in% c(5L, 10L)) {
    stop(
      "weighting_method = 'ras' is currently implemented only for income ",
      "quintiles or deciles.",
      call. = FALSE
    )
  }
  if (!all(categories %in% unique(dt$category))) {
    stop(
      "Cannot apply RAS because not all HBS categories are present in the ",
      "HBS-HICP matched data.",
      call. = FALSE
    )
  }

  category_share <- ras_category_shares(
    hbs_category = hbs_category,
    country = country,
    categories = categories,
    weight_years = sort(unique(dt$weight_year))
  )
  dt <- category_share[dt, on = .(category, weight_year)]
  if (anyNA(dt$category_share)) {
    stop("Missing group consumption shares for RAS calibration.", call. = FALSE)
  }
  dt[, hbs_multiplier := data.table::fifelse(
    consumption == 1e-6 & total_consumption == 1e-6,
    1e-6,
    consumption / total_consumption
  )]
  dt[, seed := pmax(category_share * weight * hbs_multiplier, 1e-12)]

  dt <- dt[
    ,
    ras_calibrate_group(
      .SD,
      categories = categories,
      tolerance = tolerance,
      max_iter = max_iter
    ),
    by = .(weight_year)
  ]
  dt[, category_share := NULL]
  dt[]
}

apply_ras_income_weights <- function(dt, hbs_category, categories, country = NULL,
                                     tolerance = 1e-10, max_iter = 1000L) {
  apply_ras_group_weights(
    dt = dt,
    hbs_category = hbs_category,
    categories = categories,
    country = country,
    tolerance = tolerance,
    max_iter = max_iter
  )
}

ras_calibrate_group <- function(dt, categories, tolerance, max_iter) {
  categories <- categories[categories %in% unique(dt$category)]
  coicops <- sort(unique(dt$coicop))

  wide <- dcast_ras_seed(dt, categories, coicops)
  row_targets <- dt[
    ,
    .(target = category_share[1L]),
    by = category
  ][match(categories, category), target]
  row_targets <- row_targets * 100 / sum(row_targets)
  col_targets <- dt[
    ,
    .(target = weight[1L]),
    by = coicop
  ][match(coicops, coicop), target]
  col_targets <- col_targets * 100 / sum(col_targets)

  calibrated <- ras_ipf(wide, row_targets, col_targets,
                        tolerance = tolerance, max_iter = max_iter)

  calibrated_dt <- data.table::as.data.table(as.table(calibrated))
  data.table::setnames(calibrated_dt, c("category", "coicop", "weighted_mass"))
  calibrated_dt[, `:=`(
    category = as.character(category),
    coicop = as.character(coicop)
  )]

  out <- calibrated_dt[dt, on = .(category, coicop)]
  out[, weighted_consumption := weighted_mass / category_share]
  out[, weighted_mass := NULL]
  out[]
}

ras_category_shares <- function(hbs_category, country, categories, weight_years) {
  categories <- normalize_group_labels(categories)
  n_categories <- length(categories)
  if (!hbs_category %in% names(category_data)) {
    stop("Unknown HBS category '", hbs_category, "'.", call. = FALSE)
  }
  if (is.null(country) || is.na(country) || !nzchar(country)) {
    stop("RAS group shares require a country code.", call. = FALSE)
  }

  shares <- load_group_consumption_shares()
  shares[, category := normalize_group_labels(category)]
  country_code <- toupper(country)
  shares <- shares[
    get("hbs_category") == hbs_category &
      get("country") == country_code &
      category %in% categories,
    .(category, year, category_share = group_consumption_share)
  ]
  if (nrow(shares) == 0L) {
    if (identical(hbs_category, "income") && n_categories != 5L) {
      warning(
        "RAS income group consumption-share table has no matching non-quintile ",
        "shares; using equal group shares.",
        call. = FALSE
      )
      return(data.table::CJ(
        category = categories,
        weight_year = as.integer(weight_years)
      )[, category_share := 1 / n_categories][])
    }
    stop(
      "No ", hbs_category, " group consumption shares available for RAS country '",
      country_code, "'. Run scripts/build_group_consumption_shares.R ",
      "or provide a custom weighting path.",
      call. = FALSE
    )
  }

  grid <- data.table::CJ(
    category = categories,
    weight_year = as.integer(weight_years)
  )
  out <- data.table::rbindlist(lapply(categories, function(category_i) {
    s <- shares[category == category_i]
    data.table::setorder(s, year)
    data.table::rbindlist(lapply(grid[category == category_i, weight_year], function(wy) {
      candidates <- s[year <= wy]
      if (nrow(candidates) == 0L) {
        candidates <- s[year == min(year)]
      }
      chosen <- candidates[which.max(year)]
      data.table::data.table(
        category = category_i,
        weight_year = as.integer(wy),
        category_share = chosen$category_share
      )
    }), use.names = TRUE)
  }), use.names = TRUE)
  out[
    ,
    category_share := category_share / sum(category_share),
    by = weight_year
  ]
  out[]
}

normalize_group_labels <- function(x) {
  out <- x
  unknown <- Encoding(out) == "unknown"
  if (any(unknown)) {
    out[unknown] <- iconv(out[unknown], from = "latin1", to = "UTF-8")
  }
  if (any(!unknown)) {
    out[!unknown] <- enc2utf8(out[!unknown])
  }
  missing <- is.na(out)
  if (any(missing)) {
    out[missing] <- enc2utf8(x[missing])
  }
  Encoding(out) <- "UTF-8"
  out
}

ras_income_category_shares <- function(country, categories, weight_years) {
  ras_category_shares(
    hbs_category = "income",
    country = country,
    categories = categories,
    weight_years = weight_years
  )
}

load_group_consumption_shares <- function() {
  file_name <- "group_consumption_shares.csv"
  candidates <- c(
    file.path("inst", "extdata", file_name),
    file.path("data-raw", file_name),
    system.file("extdata", file_name, package = "inflationinequality", mustWork = FALSE)
  )
  candidates <- candidates[nzchar(candidates) & file.exists(candidates)]
  if (length(candidates) == 0L) {
    return(load_income_group_consumption_shares())
  }
  out <- data.table::fread(candidates[[1L]])
  required <- c("hbs_category", "country", "year", "category", "group_consumption_share")
  missing <- setdiff(required, names(out))
  if (length(missing) > 0L) {
    stop(
      "Group consumption shares table is missing columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  out[, `:=`(
    hbs_category = as.character(hbs_category),
    country = toupper(as.character(country)),
    year = as.integer(year),
    category = as.character(category),
    group_consumption_share = as.numeric(group_consumption_share)
  )]
  out[]
}

load_income_group_consumption_shares <- function() {
  file_name <- "income_group_consumption_shares.csv"
  candidates <- c(
    file.path("inst", "extdata", file_name),
    file.path("data-raw", file_name),
    system.file("extdata", file_name, package = "inflationinequality", mustWork = FALSE)
  )
  candidates <- candidates[nzchar(candidates) & file.exists(candidates)]
  if (length(candidates) == 0L) {
    stop(
      "Missing income group consumption shares table '", file_name, "'. ",
      "Run scripts/build_income_group_consumption_shares.R first.",
      call. = FALSE
    )
  }
  out <- data.table::fread(candidates[[1L]])
  required <- c("country", "year", "category", "group_consumption_share")
  missing <- setdiff(required, names(out))
  if (length(missing) > 0L) {
    stop(
      "Income group consumption shares table is missing columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  out[, `:=`(
    hbs_category = "income",
    country = toupper(as.character(country)),
    year = as.integer(year),
    category = as.character(category),
    group_consumption_share = as.numeric(group_consumption_share)
  )]
  out
}

dcast_ras_seed <- function(dt, categories, coicops) {
  seed_dt <- dt[, .(seed = sum(seed, na.rm = TRUE)), by = .(category, coicop)]
  seed_dt[, category := factor(category, levels = categories)]
  seed_dt[, coicop := factor(coicop, levels = coicops)]
  wide <- data.table::dcast(seed_dt, category ~ coicop, value.var = "seed", fill = 1e-12)
  mat <- as.matrix(wide[, -1])
  rownames(mat) <- as.character(wide$category)
  storage.mode(mat) <- "numeric"
  mat
}

ras_ipf <- function(seed, row_targets, col_targets, tolerance, max_iter) {
  mat <- seed
  for (iter in seq_len(max_iter)) {
    row_sums <- rowSums(mat)
    mat <- mat * (row_targets / row_sums)

    col_sums <- colSums(mat)
    mat <- sweep(mat, 2L, col_sums / col_targets, "/")

    err <- max(
      abs(rowSums(mat) - row_targets),
      abs(colSums(mat) - col_targets)
    )
    if (is.finite(err) && err < tolerance) {
      return(mat)
    }
  }
  stop("RAS calibration did not converge.", call. = FALSE)
}

load_italy_level2_hbs_if_available <- function(country, category, level) {
  if (!identical(toupper(country), "IT") ||
      !category %in% c("income", "age", "urban") ||
      !identical(as.integer(level), 2L)) {
    return(NULL)
  }

  file_name <- if (identical(category, "income")) {
    "IT_income_hbs_calibrated_2015_2020_level2.rds"
  } else if (identical(category, "urban")) {
    "IT_urban_hbs_eurostat_2015_2020_level2.rds"
  } else {
    "IT_age_hbs_eurostat_2015_2020_level2.rds"
  }
  subdir <- if (identical(category, "income")) {
    "italy_calibrated_hbs"
  } else {
    "italy_hbs"
  }

  candidates <- c(
    system.file("extdata", file_name, package = "inflationinequality"),
    file.path("inst", "extdata", file_name),
    file.path("data-raw", subdir, file_name)
  )
  candidates <- candidates[nzchar(candidates) & file.exists(candidates)]
  if (length(candidates) == 0L) {
    return(NULL)
  }
  readRDS(candidates[[1L]])
}

merge_index_and_hbs <- function(index_weights, hbs, specific_hbs_year) {
  # Select COICOP codes
  hbs_coicops <- hbs$dt[nchar(coicop) == hbs$level + 1, unique(coicop)]
  weight_coicops <- index_weights$dt[nchar(coicop) == index_weights$level + 1, unique(coicop)]

  # We do not use COICOP codes that have HBS data but not CPI data
  rejected_coicops <- setdiff(hbs_coicops, weight_coicops)
  if (length(rejected_coicops) > 0) {
    message(sprintf("The following COICOP codes, found in HBS data, are removed for not being included in CPI data: %s", paste(rejected_coicops, collapse = ", ")))
  }

  # Necessary before the join
  if ("year" %in% names(index_weights$dt)) {
    data.table::setnames(index_weights$dt, "year", "weight_year")
  } else if (!"weight_year" %in% names(index_weights$dt)) {
    stop("Something's wrong!")
  }

  # COICOP codes that have CPI data but not HBS data
  missing_coicops <- setdiff(weight_coicops, hbs_coicops)

  # Extract higher level COICOP codes
  higher_coicops <- unique(substr(missing_coicops, 1, index_weights$level))

  if (length(higher_coicops) > 0) {
    pattern <- paste0("^(", paste(higher_coicops, collapse = "|"), ")")
    merged_coicops <-
      c(weight_coicops[!grepl(pattern, weight_coicops)], higher_coicops)
  } else {
    merged_coicops <- weight_coicops
  }

  if (length(missing_coicops) > 0) {
    warning(sprintf("Missing COICOPs found in CPI data but not in HBS data: %s\nReplacing these codes to higher level (%d)", paste(missing_coicops, collapse = ", "), index_weights$level - 1))
  }

  # Include total consumption column
  dt_hbs <- hbs$dt[hbs$dt_total, on = .(coicop, year)]

  # Select merged COICOPS
  dt_index_weights <- index_weights$dt[coicop %in% merged_coicops]
  dt_hbs <- dt_hbs[coicop %in% merged_coicops]

  # Select specific HBS year if applicable
  if (!is.null(specific_hbs_year)) {
    dt_hbs <- dt_hbs[year == specific_hbs_year, ]
  }

  # Now perform the Cartesian product (left join)
  dt_weighted_consumption <-
    dt_hbs[dt_index_weights, on = .(coicop), allow.cartesian = TRUE] %>%
    .[!is.na(category)
      & !is.na(weight_year)
      & !is.na(weight)
      & !is.na(year)
      & !is.na(consumption)
    ]

  return(dt_weighted_consumption)
}

merge_france_tf106_level3_index_and_hbs <- function(index_weights, hbs, specific_hbs_year) {
  dt_index_weights <- prepare_index_weights_tree(index_weights)
  dt_index_weights[, coicop := recode_coicop_ecoicop2_to_ecoicop1(coicop)]
  dt_index_weights[, coicop := coicop_to_level(coicop, 3)]
  dt_index_weights <- dt_index_weights[
    ,
    .(weight = sum(weight, na.rm = TRUE)),
    by = .(coicop, weight_year)
  ]
  dt_hbs <- hbs$dt[hbs$dt_total, on = .(coicop, year)]

  if (!is.null(specific_hbs_year)) {
    dt_hbs <- dt_hbs[year == specific_hbs_year, ]
  }

  tf106_codes <- france_tf106_level3_codes()
  hbs_coicops <- unique(dt_hbs$coicop)
  dt_index_weights[, tf106_coicop := closest_available_hbs_coicop(coicop, tf106_codes)]
  dt_index_weights[, hbs_coicop := closest_available_hbs_coicop(tf106_coicop, hbs_coicops)]

  missing_hbs_match <- dt_index_weights[is.na(hbs_coicop), unique(coicop)]
  if (length(missing_hbs_match) > 0) {
    stop(
      "French level-3 HICP codes found in CPI weights but not in TF106/HBS, ",
      "even after rolling up to an available TF106 parent: ",
      paste(missing_hbs_match, collapse = ", ")
    )
  }

  dt_index_weights <- dt_index_weights[
    ,
    .(weight = sum(weight, na.rm = TRUE)),
    by = .(coicop = hbs_coicop, weight_year)
  ]

  dt_hbs[dt_index_weights, on = .(coicop), allow.cartesian = TRUE] %>%
    .[!is.na(category)
      & !is.na(weight_year)
      & !is.na(weight)
      & !is.na(year)
      & !is.na(consumption)
    ]
}

merge_spain_epf_level3_index_and_hbs <- function(index_weights, hbs, specific_hbs_year) {
  dt_index_weights <- prepare_index_weights_tree(index_weights)
  dt_index_weights[, coicop := recode_coicop_ecoicop2_to_ecoicop1(coicop)]
  dt_index_weights[, coicop := coicop_to_level(coicop, 3)]
  dt_index_weights <- dt_index_weights[
    ,
    .(weight = sum(weight, na.rm = TRUE)),
    by = .(coicop, weight_year)
  ]
  dt_hbs <- hbs$dt[hbs$dt_total, on = .(coicop, year)]

  if (!is.null(specific_hbs_year)) {
    dt_hbs <- dt_hbs[year == specific_hbs_year, ]
  }

  hbs_coicops <- unique(dt_hbs$coicop)
  dt_index_weights[, hbs_coicop := closest_available_hbs_coicop(coicop, hbs_coicops)]

  missing_hbs_match <- dt_index_weights[is.na(hbs_coicop), unique(coicop)]
  if (length(missing_hbs_match) > 0) {
    stop(
      "Spain level-3 HICP codes found in CPI weights but not in EPF HBS, ",
      "even after rolling up to an available parent: ",
      paste(missing_hbs_match, collapse = ", ")
    )
  }

  dt_index_weights <- dt_index_weights[
    ,
    .(weight = sum(weight, na.rm = TRUE)),
    by = .(coicop = hbs_coicop, weight_year)
  ]

  dt_hbs[dt_index_weights, on = .(coicop), allow.cartesian = TRUE] %>%
    .[!is.na(category)
      & !is.na(weight_year)
      & !is.na(weight)
      & !is.na(year)
      & !is.na(consumption)
    ]
}

prepare_index_weights_tree <- function(index_weights) {
  dt <- data.table::copy(index_weights$dt)
  if ("year" %in% names(dt)) {
    data.table::setnames(dt, "year", "weight_year")
  } else if (!"weight_year" %in% names(dt)) {
    stop("Index weights must contain either 'year' or 'weight_year'.")
  }

  dt <- dt[
    ,
    .(weight = sum(weight, na.rm = TRUE)),
    by = .(coicop, weight_year)
  ]
  dt <- dt[!is.na(coicop) & !is.na(weight_year) & !is.na(weight)]
  if (nrow(dt) == 0) {
    return(dt)
  }

  tree_keep <- hicp::tree(
    id = dt$coicop,
    by = dt$weight_year,
    w = dt$weight,
    flag = TRUE,
    settings = list(
      coicop.prefix = "",
      all.items.code = "00",
      max.lvl = index_weights$level + 1L,
      w.tol = 1 / 100,
      chatty = FALSE
    )
  )
  dt[tree_keep]
}

france_tf106_level3_codes <- function() {
  path <- system.file("extdata", "TF106_3digit.xlsx", package = "inflationinequality", mustWork = FALSE)
  if (!nzchar(path)) {
    path <- file.path("vignettes", "TF106_3digit.xlsx")
  }
  if (!file.exists(path)) {
    stop("France level-3 mapping requires TF106_3digit.xlsx, but it could not be found.")
  }
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("Package 'readxl' is required to read TF106_3digit.xlsx.", call. = FALSE)
  }
  dt <- data.table::as.data.table(readxl::read_excel(path, sheet = "TF106", col_types = "text"))
  sort(unique(stats::na.omit(as.character(dt[[1L]]))))
}

closest_available_hbs_coicop <- function(coicop, hbs_coicops) {
  vapply(coicop, function(code) {
    if (is.na(code)) {
      return(NA_character_)
    }
    candidates <- vapply(seq.int(nchar(code), 2L), function(last) {
      substr(code, 1L, last)
    }, character(1L))
    match_index <- match(candidates, hbs_coicops, nomatch = 0L)
    if (!any(match_index > 0L)) {
      NA_character_
    } else {
      candidates[which(match_index > 0L)[[1L]]]
    }
  }, character(1L), USE.NAMES = FALSE)
}
