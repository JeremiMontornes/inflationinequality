#' Calculates inflation contributions
#'
#' @description
#' `calculate_contributions()` computes the contributions to inflation for
#' different COICOP (Classification of Individual Consumption According to
#' Purpose) categories over time, using Consumer Price Index (CPI) data and
#' weighted consumption data.
#'
#' @details
#' The function performs the following key operations:
#' 1. Loads CPI data and calculates weights using `load_cpi()` and
#' `calculate_weights()`.
#' 2. Filters data to ensure consistency across COICOP codes and years.
#' 3. Calculates contributions using a formula that accounts for year-over-year
#' changes in prices and weights.
#' 4. Handles multiple categories (e.g., income groups, age groups)
#' simultaneously.
#'
#' The contribution calculation is based on the following formula:
#' Contribution = (P_{y-1,12} / P_{y-1,m}) * w_{y-1,j,q} * ((P_{y,m,j} - P_{y-1,12,j}) / P_{y-1,12,j}) +
#'                (P_{y-2,12} / P_{y-1,m}) * w_{y-2,j,q} * ((P_{y-1,12,j} - P_{y-1,m,j}) / P_{y-2,12,j})
#'
#' Where:
#' * P: Price index
#' * w: Weight
#' * y: Year
#' * m: Month
#' * j: COICOP category
#' * q: Demographic category (e.g., income quintile)
#'
#' @inheritParams calculate_weights
#' @param start_month month of start date.
#' @param end_month month of end date.
#' @param ensure_complete_cpi flag that when set to `TRUE`, synthesizes missing
#'   CPI data when the CPI dataset is incomplete.
#' @param custom_cpi an object of class `"cpi"`
#'
#' @returns An object of class `"contributions"` is a list containing the
#'   following components:
#' - `dt`: a `data.table` object (see below).
#' - `country`: 2-digit country code (see ISO 3166-1 alpha-2).
#' - `category`: Category for which contributions were calculated: "income",
#' "age", or "urban".
#' - `categories`: (Ordered) vector of category types, from lowest to highest.
#' - `level`: COICOP level.
#' - `start_year`: first year of data.
#' - `start_month`: first month of data.
#' - `end_year`: last year of data.
#' - `end_month`: last month of data.
#'
#' The component `dt` has the following columns:
#'
#' - `year`: year of the contribution.
#' - `coicop`: COICOP code.
#' - `month`: month of the contribution.
#' - `category`: demographic category (e.g., income group, age group).
#' - `contribution`: calculated contribution to inflation.
#'
#' @examples
#' # Calculate inflation contributions for France, income category, COICOP level
#' # 2, from 2010 to 2020
#' france_contributions <- calculate_contributions("FR", "income", level = 2,
#' start_year = 2010, end_year = 2020)
#'
#' # Access the data.table component
#' dt_contributions <- france_contributions$dt
#'
#' # Sum contributions for a specific year, month, and category
#' total_contribution <- dt_contributions[year == 2015 &&
#' month == 6 &&
#' category == "First quintile",
#' sum(contribution)]
#'
#' @seealso [load_cpi()], [calculate_weights()], [correct_cpi()] for CPI data
#'   synthesization, [cpi()]
#'
#' @importFrom data.table :=
#' @export
calculate_contributions <- function(country = NULL, category = NULL, level = 2,
                                    start_year = NULL, start_month = NULL,
                                    end_year = NULL, end_month = NULL,
                                    ensure_complete_cpi = FALSE,
                                    custom_cpi = NULL,
                                    custom_index_weights = NULL,
                                    custom_hbs = NULL,
                                    interpolated_hbs = FALSE,
                                    specific_hbs_year = NULL) {
  # Input validation
  if (!is.character(country) || nchar(country) != 2) {
    stop("Country must be a 2-character ISO code")
  }
  if (!category %in% c("income", "age", "urban")) {
    stop("Category must be one of 'income', 'age', or 'urban'")
  }
  if (!is.numeric(level) || !level %in% 1:3) {
    stop("Level must be an integer between 1 and 3")
  }

  # Set start year 2 years behind due to the requirements of the equation
  data_start_year <- if (!is.null(start_year)) {
    start_year - 2
  } else {
    start_year
  }

  # Load CPI data
  cpi <- if (is.null(custom_cpi)) {
    if (is.null(country)) {
      stop("Either 'country' or 'custom_cpi' must be provided.")
    }
    load_cpi(
      country, level = level,
      start_year = data_start_year, start_month = start_month,
      end_year = end_year, end_month = end_month)
  } else {
    # Check if date range is sufficient
    if (!is.null(data_start_year)) {
      if (data_start_year < custom_cpi$start_year) {
        stop(paste0("Not enough CPI data. Latest possible start year: ", data_start_year))
      }
      if (!is.null(start_month)
          && data_start_year == custom_cpi$start_year
          && start_month < custom_cpi$start_month) {
        stop(paste0("Not enough CPI data. Latest possible start date: ", data_start_year, "-", start_month))
      }
    }

    if (!is.null(end_year)) {
      if (end_year > custom_cpi$start_year) {
        stop(paste0("Not enough CPI data. Earliest possible end year: ", end_year))
      }
      if (!is.null(end_month)
          && end_year == custom_cpi$end_year
          && end_month > custom_cpi$end_month) {
        stop(paste0("Not enough CPI data. Earliest possible end date: ", end_year, "-", end_year))
      }
    }

    custom_cpi
  }

  weights <-
    calculate_weights(
      country, category,
      level = level,
      start_year = data_start_year, end_year = end_year,
      custom_index_weights = custom_index_weights,
      custom_hbs = custom_hbs,
      interpolated_hbs = interpolated_hbs,
      specific_hbs_year = specific_hbs_year)

  # Definitely set start_year and end_year for the ticker
  start_year <- if (is.null(start_year) || start_year < cpi$start_year) {
    cpi$start_year
  } else {
    start_year
  }

  end_year <- if (is.null(end_year) || end_year > cpi$end_year) {
    cpi$end_year
  } else {
    end_year
  }

  # Select COICOP codes
  cpi_coicops <- unique(cpi$dt$coicop)
  weight_coicops <- unique(weights$dt$coicop)

  # We do not use COICOP codes that have HBS data but not CPI data
  weights$dt <- weights$dt[coicop %in% cpi_coicops, ]
  rejected_coicops <- setdiff(weight_coicops, cpi_coicops)
  if (length(rejected_coicops) > 0) {
    message(sprintf("The following COICOP codes, found in HBS data, are removed for not being included in CPI data: %s", paste(rejected_coicops, collapse = ", ")))
  }

  # Synthesize missing CPI data
  if (ensure_complete_cpi) {
    cpi <- correct_cpi(cpi)
  }

  # ----------------------------------------------------------------------------

  # The goal of this section is to ensure that all dates in CPI for each COICOP
  # code has a weight associated to it

  # Create a sequence of all valid year-month combinations
  all_dates <- data.table::data.table(
    year = rep((cpi$start_year):(cpi$end_year), each = 12),
    month = rep(1:12, times = cpi$end_year - cpi$start_year + 1)
  )
  all_dates <- all_dates[year < cpi$end_year | (year == cpi$end_year & month <= cpi$end_month)]

  # Create a complete grid of all combinations
  weight_complete_grid <- data.table::CJ(coicop = cpi_coicops,
                                         category = weights$categories,
                                         weight_year = all_dates$year,
                                         unique = TRUE)

  # Merge the complete grid with the original data
  weights_result <- merge(weight_complete_grid, weights$dt, by = c("coicop", "category", "weight_year"), all.x = TRUE)

  # Fill in missing values
  weights_result[, weighted_consumption := pmax(weighted_consumption, 1e-6, na.rm = TRUE)]

  # If you want to keep only the columns from the original data.table
  weights$dt <- weights_result[, .(series_name, coicop, category, weight_year, year, weighted_consumption)]

  # ----------------------------------------------------------------------------

  contrib2 <- data.table::data.table(
    year = numeric(),
    coicop = character(),
    month = numeric(),
    category = character(),
    annee1 = numeric(),
    annee0 = numeric(),
    inflation_annee1 = numeric(),
    inflation_annee0 = numeric(),
    contrib_annee1 = numeric(),
    contrib_annee0 = numeric(),
    contribution = numeric()
  )

  dt_missing_weights <- data.table::data.table(
    coicop = character(),
    year = numeric(),
    category = character(),
    missing_weight = numeric()
  )

  pb <- progress::progress_bar$new(
    format = "calculating contributions [:bar] :percent eta: :eta (elapsed: :elapsed)",
    total = length(start_year:end_year), clear = FALSE
  )
  pb$tick(0)

  categories <- unique(weights$dt$category)

  for (y in start_year:end_year) {
    # Price index of basket
    p_y1_12 <- cpi$dt_basket[month == 12 & year == y - 1, value]
    p_y2_12 <- cpi$dt_basket[month == 12 & year == y - 2, value]

    # We only use COICOP codes that exist in years `y-2`, `y-1`, `y`
    # but also have more than 1e-6 in those years.
    valid_coicops <- cpi$dt[year %in% c(y-2, y-1, y),
                            .(n_years = data.table::uniqueN(year),
                              all_valid = all(value > 1e-6)),
                            by = coicop][n_years == 3 & all_valid == TRUE, unique(coicop)]

    # COICOP codes that do not exist that recorded
    missing_coicops_y <- setdiff(cpi_coicops, valid_coicops)

    # This can be further optimised since each COICOP code is independent
    for (j in valid_coicops) {
      # Constant values
      p_y1_12_j <- cpi$dt[coicop == j &
        month == 12 &
        year == y - 1, value]
      p_y2_12_j <- cpi$dt[coicop == j &
        month == 12 &
        year == y - 2, value]

      # Calculate HBS weights across all categories
      w_y1_j_q <- weights$dt[coicop == j &
        weight_year == y, .(category, weighted_consumption)]
      data.table::setkey(w_y1_j_q, category)
      w_y2_j_q <- weights$dt[coicop == j &
        weight_year == y - 1, .(category, weighted_consumption)]
      data.table::setkey(w_y2_j_q, category)

      # Calculate index weights across all months
      # This is a vector
      p_y1_m <- cpi$dt_basket[year == y - 1, sum(value), by = .(month)]
      data.table::setkey(p_y1_m, month)
      # (not necessarily 12 for the latest year)

      # Note: there is a shift in year: y-2 is actually y-1 and y-1 is actually y!!!
      p_y1_m_j <- cpi$dt[coicop == j &
        year == y - 1, .(month, value)]
      p_y_m_j <- cpi$dt[coicop == j &
        year == y, .(month, value)]

      # Cross join month and category
      dt_cj <- data.table::CJ(
        month = p_y1_m$month,
        category = w_y1_j_q$category
      )

      # Fill up columns according to their respective keys
      dt_cj[, w_y1_j_q := w_y1_j_q[.SD, on = .(category), weighted_consumption]]
      dt_cj[, w_y2_j_q := w_y2_j_q[.SD, on = .(category), weighted_consumption]]
      dt_cj[, p_y1_m := p_y1_m[.SD, on = .(month), V1]]
      dt_cj[, p_y1_m_j := p_y1_m_j[.SD, on = .(month), value]]
      dt_cj[, p_y_m_j := p_y_m_j[.SD, on = .(month), value]]

      # Apply formula
      dt_cj[, inflation_annee1 := 100 * (p_y_m_j / p_y1_12_j - 1)]
      dt_cj[, inflation_annee0 := 100 * ((p_y1_12_j - p_y1_m_j) / p_y2_12_j)]

      dt_cj[, contrib_annee1 := (p_y1_12 / p_y1_m) * w_y1_j_q * inflation_annee1]
      dt_cj[, contrib_annee0 := (p_y2_12 / p_y1_m) * w_y2_j_q * inflation_annee0]

      dt_cj[, contribution := (contrib_annee1 + contrib_annee0) / 100]

      # Clean up
      dt_cj[, `:=`(year = y, coicop = j)]
      dt_cj <- dt_cj[!is.na(contribution), .(year, coicop, month, category,
                                             annee1 = w_y1_j_q, annee0 = w_y2_j_q,
                                             inflation_annee1, inflation_annee0,
                                             contrib_annee1, contrib_annee0,
                                             contribution)]

      # Bind the new contributions to the existing data table
      contrib2 <- data.table::rbindlist(list(contrib2, dt_cj), use.names = TRUE)
    }
    dt_missing_weights_y <- weights$dt[coicop %in% missing_coicops_y
                                       & weight_year == y,
                                       .(coicop, year = weight_year, category,
                                         missing_weight = weighted_consumption)]

    dt_missing_weights <- data.table::rbindlist(list(dt_missing_weights, dt_missing_weights_y),
                                            use.names = TRUE)

    pb$tick()
  }

  # Drop useless columns
  contrib2 <- contrib2[, .(coicop, category, year, month, contribution)]

  dt_significant_missing_weights <- dt_missing_weights[, .(average_missing_weight = mean(missing_weight)),
                                                       by = .(coicop, year)]
  dt_significant_missing_weights <- dt_significant_missing_weights[average_missing_weight >= 1, ]

  if (nrow(dt_significant_missing_weights) > 0) {
    message(
      "There are significant weights (>=1%) that are not included:\n",
      paste(capture.output(print(dt_significant_missing_weights)),
            collapse = "\n"))
  }

  return(structure(list(dt = contrib2,
                        dt_missing_weight = dt_missing_weights,
                        country = country,
                        category = category,
                        categories = weights$categories,
                        level = level,
                        start_year = min(contrib2$year),
                        start_month = 1,
                        end_year = max(contrib2$year),
                        end_month = max(contrib2[year == max(contrib2$year), month])),
                   class = "contributions"))
}
