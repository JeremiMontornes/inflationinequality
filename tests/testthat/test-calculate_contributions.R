#' Helper function to check internet connectivity
has_internet <- function() {
  tryCatch({
    readLines("https://cran.r-project.org", n = 1)
    TRUE
  }, warning = function(w) FALSE,
  error = function(e) FALSE)
}


#' Custom skip function
skip_if_no_internet <- function() {
  testthat::skip_on_cran()
  if (identical(Sys.getenv("INFLATIONINEQUALITY_SKIP_NETWORK_TESTS"), "true")) {
    skip("Network tests disabled")
  }
  if (!has_internet()) {
    skip("No internet connection")
  }
}

# Mock functions to simulate data loading
mock_load_cpi <- function(country, level, start_year, start_month, end_year, end_month) {
  cpi_fr2 <- if (!is.null(start_year) &&
                 start_year == 2016 &&
                 !is.null(end_year) &&
                 end_year == 2017) {
    readRDS(test_path("fixtures", "cpi_fr2_s2016_e2017.RDS"))
  } else if (!is.null(start_year) &&
             start_year == 2014 &&
             !is.null(end_year) &&
             end_year == 2017) {
    readRDS(test_path("fixtures", "cpi_fr2_s2014_e2017.RDS"))
  } else {
    readRDS("fixtures/cpi_fr2.RDS")
  }
  return(cpi_fr2)
}

mock_calculate_weights <- function(country, category, level, start_year, end_year,
                                   custom_index_weights,
                                   custom_hbs,
                                   interpolated_hbs,
                                   specific_hbs_year,
                                   france_insee_income_groups = "decile",
                                   weighting_method = "relative_expenditure") {
  weights_fr2 <- if (category == "income") {
    if (!is.null(start_year) &&
        start_year == 2016 &&
        !is.null(end_year) &&
        end_year == 2017) {
      readRDS("fixtures/weights_fr_income2_s2016_e2017.RDS")
    } else if (!is.null(start_year) &&
               start_year == 2014 &&
               !is.null(end_year) &&
               end_year == 2017) {
      readRDS("fixtures/weights_fr_income2_s2014_e2017.RDS")
    } else {
      readRDS("fixtures/weights_fr_income2.RDS")
    }
  } else if (category == "age") {
    readRDS("fixtures/weights_fr_age2.RDS")
  } else {
    readRDS("fixtures/weights_fr_urban2.RDS")
  }
  return(weights_fr2)
}

mock_load_index_weights_contributions <- function(country, level, start_year,
                                                   end_year) {
  readRDS("fixtures/index_weights_fr2.RDS")
}

test_that("calculate_contributions input validation works", {
  skip_if_no_internet()
  expect_error(calculate_contributions("FRA", "income"), "Country must be a 2-character ISO code")
  expect_error(calculate_contributions("FR", "invalid"), "Category must be one of 'income', 'age', or 'urban'")
  expect_error(calculate_contributions("FR", "income", level = 4), "Level must be an integer between 1 and 3")
})

test_that("calculate_contributions returns expected structure", {
  local_mocked_bindings(load_cpi = mock_load_cpi, .package = "inflationinequality")
  local_mocked_bindings(load_index_weights = mock_load_index_weights_contributions,
                        .package = "inflationinequality")
  local_mocked_bindings(calculate_weights = mock_calculate_weights, .package = "inflationinequality")
  result <- calculate_contributions(
    "FR", "income", recode_ecoicop2_to_ecoicop1 = FALSE
  )
  expect_s3_class(result$dt, "data.table")
  expect_named(result$dt, c("coicop", "category", "year", "month", "contribution"))
})

test_that("calculate_contributions handles different date ranges", {
  local_mocked_bindings(load_cpi = mock_load_cpi, .package = "inflationinequality")
  local_mocked_bindings(load_index_weights = mock_load_index_weights_contributions,
                        .package = "inflationinequality")
  local_mocked_bindings(calculate_weights = mock_calculate_weights, .package = "inflationinequality")
  result_full <- calculate_contributions(
    "FR", "income", recode_ecoicop2_to_ecoicop1 = FALSE
  )
  result_partial <- calculate_contributions(
    "FR", "income", start_year = 2016, end_year = 2017,
    recode_ecoicop2_to_ecoicop1 = FALSE
  )

  expect_lte(nrow(result_partial$dt), nrow(result_full$dt))
  expect_in(result_partial$dt$year, 2016:2017)  # Remember, it calculates for y-2 years
})

test_that("calculate_contributions works with different categories", {
  local_mocked_bindings(load_cpi = mock_load_cpi, .package = "inflationinequality")
  local_mocked_bindings(load_index_weights = mock_load_index_weights_contributions,
                        .package = "inflationinequality")
  local_mocked_bindings(calculate_weights = mock_calculate_weights, .package = "inflationinequality")
  result_income <- calculate_contributions(
    "FR", "income", recode_ecoicop2_to_ecoicop1 = FALSE
  )
  result_age <- calculate_contributions(
    "FR", "age", recode_ecoicop2_to_ecoicop1 = FALSE
  )
  result_urban <- calculate_contributions(
    "FR", "urban", recode_ecoicop2_to_ecoicop1 = FALSE
  )

  expect_false(identical(result_income$dt, result_urban$dt))
})

test_that("calculate_contributions with sideloaded CPI data fails with mismatched dates", {
  local_mocked_bindings(load_cpi = mock_load_cpi, .package = "inflationinequality")
  local_mocked_bindings(load_index_weights = mock_load_index_weights_contributions,
                        .package = "inflationinequality")
  local_mocked_bindings(calculate_weights = mock_calculate_weights, .package = "inflationinequality")
  dt_cpi_fr <- load_cpi("FR",start_year = 2016, end_year = 2017)
  expect_error(calculate_contributions("FR", "income", start_year = 2013, custom_cpi = dt_cpi_fr))
})

test_that("elementary contributions exactly decompose the displayed annual rate", {
  dt <- data.table::CJ(
    coicop = c("01", "02"),
    category = c("Q1", "Q5"),
    year = 2021:2022,
    month = 1:12
  )
  dt[, date := as.Date(sprintf("%04d-%02d-01", year, month))]
  dt[, dec_ratio := 1 + ifelse(coicop == "01", 0.01, 0.02) * month]
  dt[, weighted_consumption := data.table::fcase(
    category == "Q1" & coicop == "01", 60,
    category == "Q1", 40,
    coicop == "01", 40,
    default = 60
  )]

  built <- inflationinequality:::build_index_components(dt, base_year = 2021)
  sums <- built$components[
    , .(component_sum = sum(contribution)),
    by = .(category, year, month)
  ]
  checked <- built$index[sums, on = .(category, year, month)]

  expect_lt(
    max(abs(checked[is.finite(annual_rate), component_sum - annual_rate])),
    1e-10
  )
  expect_equal(
    built$effective_weights[, sum(effective_weight), by = .(category, year)]$V1,
    rep(100, 4),
    tolerance = 1e-12
  )
})

test_that("policy contributions decompose the observed-counterfactual group gap", {
  make_data <- function(policy_factor = 1) {
    dt <- data.table::CJ(
      coicop = c("01", "02"),
      category = c("Q1", "Q5"),
      year = 2021:2022,
      month = 1:12
    )
    dt[, date := as.Date(sprintf("%04d-%02d-01", year, month))]
    dt[, dec_ratio := 1 + ifelse(coicop == "01", 0.01, 0.02) * month]
    dt[coicop == "02" & year == 2022, dec_ratio :=
         1 + policy_factor * (dec_ratio - 1)]
    dt[, weighted_consumption := data.table::fcase(
      category == "Q1" & coicop == "01", 70,
      category == "Q1", 30,
      coicop == "01", 30,
      default = 70
    )]
    dt
  }
  observed <- inflationinequality:::build_index_components(
    make_data(1), base_year = 2021
  )
  counterfactual <- inflationinequality:::build_index_components(
    make_data(0.5), base_year = 2021
  )
  expect_equal(observed$effective_weights, counterfactual$effective_weights)
  expect_equal(observed$coverage, counterfactual$coverage)

  rates <- merge(
    observed$index[, .(category, year, month, observed_rate = annual_rate)],
    counterfactual$index[, .(category, year, month,
                             counterfactual_rate = annual_rate)],
    by = c("category", "year", "month")
  )
  rate_gap <- rates[year == 2022 & month == 6,
    diff((observed_rate - counterfactual_rate)[match(c("Q1", "Q5"), category)])
  ]
  product_gap <- merge(
    observed$components[, .(coicop, category, year, month,
                             observed = contribution)],
    counterfactual$components[, .(coicop, category, year, month,
                                   counterfactual = contribution)],
    by = c("coicop", "category", "year", "month")
  )[year == 2022 & month == 6,
    sum((observed - counterfactual) * ifelse(category == "Q5", 1, -1))
  ]
  expect_equal(product_gap, rate_gap, tolerance = 1e-10)
})

test_that("calculate_contributions does not mix up data between categories: single category", {
  skip_if_no_internet()
  hbs <- load_hbs("FR", "income")
  dt_reduced_hbs <- hbs$dt[category == "Fifth quintile"]
  reduced_hbs <- hbs(
    dt = dt_reduced_hbs, dt_total = hbs$dt_total,
    country = hbs$category, category = hbs$category,
    categories = "Fifth quintile", level = 2
  )

  contributions <- calculate_contributions("FR", "income", custom_hbs = hbs)
  reduced_contributions <- calculate_contributions("FR", "income", custom_hbs = reduced_hbs)

  dt_contributions <- contributions$dt[category == "Fifth quintile"]
  dt_reduced_contributions <- reduced_contributions$dt[, .(coicop, year, month, category, reduced_contribution = contribution)]

  # Merge the two data.tables
  merged_dt <-
    dt_contributions[dt_reduced_contributions, on = .(coicop, year, month, category)]

  # Compare contributions
  merged_dt[, match := contribution == reduced_contribution]

  # Check for mismatches
  mismatches <- merged_dt[match == FALSE | is.na(match)]

  # Test assertion
  expect_equal(nrow(mismatches), 0,
               info = "Mismatches found in contributions between the two data.tables")
})

test_that("calculate_contributions does not mix up data between categories: random order of categories", {
  skip_if_no_internet()
  hbs <- load_hbs("FR", "income")
  reduced_hbs <- hbs(
    dt = hbs$dt, dt_total = hbs$dt_total,
    country = hbs$category, category = hbs$category,
    categories = c("Third quintile", "Fifth quintile", "Fourth quintile", "Second quintile", "First quintile"), level = 2
  )

  contributions <- calculate_contributions("FR", "income", custom_hbs = hbs)
  reduced_contributions <- calculate_contributions("FR", "income", custom_hbs = reduced_hbs)

  dt_contributions <- contributions$dt
  dt_reduced_contributions <- reduced_contributions$dt[, .(coicop, year, month, category, reduced_contribution = contribution)]

  # Merge the two data.tables
  merged_dt <-
    dt_contributions[dt_reduced_contributions, on = .(coicop, year, month, category)]

  # Compare contributions
  merged_dt[, match := contribution == reduced_contribution]

  # Check for mismatches
  mismatches <- merged_dt[match == FALSE | is.na(match)]

  # Test assertion
  expect_equal(nrow(mismatches), 0,
               info = "Mismatches found in contributions between the two data.tables")
})
