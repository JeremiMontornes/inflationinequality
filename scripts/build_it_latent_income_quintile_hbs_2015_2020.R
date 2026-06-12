suppressPackageStartupMessages({
  library(data.table)
  library(inflationinequality)
})

out_dir <- file.path("data-raw", "italy_latent_income_hbs")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
zip_dir <- file.path("data-raw", "italy_hbs", "zips")
dir.create(zip_dir, recursive = TRUE, showWarnings = FALSE)

istat_dir <- Sys.getenv(
  "ISTAT_HBS_DIR",
  "C:/Users/jerem/Documents/New project/data/istat_hbs"
)

ensure_repo_zip <- function(file_name) {
  repo_zip <- file.path(zip_dir, file_name)
  if (file.exists(repo_zip)) {
    return(repo_zip)
  }
  source_zip <- file.path(istat_dir, file_name)
  if (!file.exists(source_zip)) {
    stop("Missing Istat ZIP: ", source_zip, call. = FALSE)
  }
  file.copy(source_zip, repo_zip, overwrite = TRUE)
  repo_zip
}

cfg <- list(
  target_years = c(2015L, 2020L),
  level = 2L,
  # COICOP baskets are estimated by the latent model. Aggregate consumption
  # shares by income quintile are targeted by reweighting expenditure-quintile
  # totals with the inverse average propensity to consume.
  calibration_year = 2005L,
  maxit = 4L,
  tau = 85,
  target_weight = 0,
  group_target_weight = 1,
  ridge = 0.01,
  seed = 20260528L,
  zips = c(
    "2015" = ensure_repo_zip("HBS_2015_IT.zip"),
    "2020" = ensure_repo_zip("HBS_2020_IT.zip")
  )
)

zip_member <- function(zip_file, pattern = "MICRODATI/.*\\.txt$") {
  members <- utils::unzip(zip_file, list = TRUE)
  hit <- members$Name[grepl(pattern, members$Name, ignore.case = TRUE)]
  if (length(hit) == 0L) {
    stop("No ZIP member matched pattern: ", pattern, call. = FALSE)
  }
  hit[[1L]]
}

fread_zip_member <- function(zip_file, member, select = NULL, ...) {
  out_dir <- tempfile("istat_hbs_unzip_")
  dir.create(out_dir)
  on.exit(unlink(out_dir, recursive = TRUE, force = TRUE), add = TRUE)
  utils::unzip(zip_file, files = member, exdir = out_dir, junkpaths = TRUE)
  extracted <- file.path(out_dir, basename(member))
  data.table::fread(extracted, select = select, ...)
}

available_names <- function(zip_file) {
  member <- zip_member(zip_file)
  names(fread_zip_member(zip_file, member, nrows = 0L))
}

coalesce_col <- function(dt, candidates, default = NA_real_) {
  hit <- intersect(candidates, names(dt))
  if (length(hit) == 0L) {
    return(rep(default, nrow(dt)))
  }
  dt[[hit[[1L]]]]
}

as_num <- function(x) suppressWarnings(as.numeric(x))

carbonaro_scale <- function(n) {
  n <- as_num(n)
  fifelse(n <= 1, 0.60,
    fifelse(n == 2, 1.00,
      fifelse(n == 3, 1.33,
        fifelse(n == 4, 1.63,
          fifelse(n == 5, 1.90, fifelse(n == 6, 2.15, 2.40))
        )
      )
    )
  )
}

weighted_quintile <- function(x, w) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  out <- rep(NA_integer_, length(x))
  if (!any(ok)) return(out)
  ord <- order(x[ok], seq_along(x[ok]))
  p <- cumsum(w[ok][ord]) / sum(w[ok][ord])
  out[which(ok)[ord]] <- findInterval(p, c(0.2, 0.4, 0.6, 0.8)) + 1L
  out
}

weighted_ntile_rank <- function(x, w) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  out <- rep(NA_real_, length(x))
  if (!any(ok)) return(out)
  ord <- order(x[ok], seq_along(x[ok]))
  ww <- w[ok][ord]
  r <- (cumsum(ww) - 0.5 * ww) / sum(ww)
  out[which(ok)[ord]] <- pmin(pmax(r, 1e-6), 1 - 1e-6)
  out
}

quintile_probabilities <- function(score, w, tau = 85) {
  r <- weighted_ntile_rank(score, w)
  centers <- seq(0.1, 0.9, by = 0.2)
  eta <- outer(r, centers, function(a, b) -tau * (a - b)^2)
  eta <- eta - apply(eta, 1L, max, na.rm = TRUE)
  p <- exp(eta)
  p <- p / rowSums(p)
  p[!is.finite(r), ] <- NA_real_
  colnames(p) <- paste0("Q", 1:5)
  p
}

division_cols <- function(names_vec) {
  cols <- c(
    sprintf("d_%02d_rico", 1:12), "d_04_str_rico",
    "d_01", "d_02", "d_03", "d_04_str", "d_05", "d_06_aggr_1",
    "d_07", "d_08", "d_09", "d_10", "d_11", "d_12"
  )
  intersect(cols, names_vec)
}

standardize <- function(x) {
  x <- as_num(x)
  s <- stats::sd(x, na.rm = TRUE)
  m <- mean(x, na.rm = TRUE)
  if (!is.finite(s) || s == 0) {
    return(rep(0, length(x)))
  }
  out <- (x - m) / s
  out[!is.finite(out)] <- 0
  out
}

build_household_features <- function(dt) {
  ncomp <- as_num(coalesce_col(dt, c("c_Ncmp_altro", "ncomp", "NCOMP"), 1))
  ncomp[!is.finite(ncomp) | ncomp <= 0] <- 1
  div_cols <- attr(dt, "division_cols")
  div_cols <- div_cols[div_cols %in% names(dt)]
  total_consumption <- rowSums(dt[, ..div_cols], na.rm = TRUE)
  age_ref <- as_num(coalesce_col(dt, c("c_c_etacalc_1", "ETA_PR"), NA))
  education <- as_num(coalesce_col(dt, c("c_titstu_1", "TITSTU_PR"), NA))
  employment <- as_num(coalesce_col(dt, c("c_cond_1", "condFL_1"), NA))
  professional <- as_num(coalesce_col(dt, c("c_pospro_1", "c_profess1dig_1"), NA))
  tenure <- as_num(coalesce_col(dt, c("Titoccup", "Propabit", "tipabitaz_new"), NA))
  surface <- as_num(coalesce_col(dt, c("c_Superf", "c_Superf_sec"), NA))
  rooms <- as_num(coalesce_col(dt, c("c_Stanze", "c_Stanze_sec"), NA))
  car <- as_num(coalesce_col(dt, c("Possauto", "Numauto_topcod"), 0))
  pc <- as_num(coalesce_col(dt, c("Posspc", "Numpc_topcod"), 0))
  internet <- as_num(coalesce_col(dt, c("Internet"), 0))
  econ_resources <- as_num(coalesce_col(dt, c("Risecon"), NA))
  econ_situation <- as_num(coalesce_col(dt, c("Sitecon"), NA))
  poverty <- as_num(coalesce_col(dt, c("povassc_rico", "poveri_rico", "povassc", "poveri"), NA))
  x <- data.table(
    consumption_level = standardize(log1p(total_consumption / ncomp)),
    age_ref = standardize(age_ref),
    education = standardize(education),
    employment = standardize(employment),
    professional = standardize(professional),
    tenure = standardize(tenure),
    surface_pp = standardize(surface / ncomp),
    car = standardize(car > 0),
    pc = standardize(pc > 0),
    internet = standardize(internet > 0),
    econ_resources = standardize(econ_resources),
    econ_situation = standardize(econ_situation),
    poverty = standardize(poverty)
  )

  mm <- stats::model.matrix(
    ~ consumption_level + age_ref + education + employment +
      professional + tenure + surface_pp + car + pc + internet +
      econ_resources + econ_situation + poverty,
    data = as.data.frame(x)
  )
  mm[, colSums(abs(mm), na.rm = TRUE) > 0, drop = FALSE]
}

read_istat_hbs_year <- function(year, zip_file) {
  nms <- available_names(zip_file)
  div_cols <- division_cols(nms)
  needed <- unique(c(
    "w_anno_rico", "w_anno", "c_Ncmp_altro", "c_c_etacalc_1", "c_titstu_1",
    "c_cond_1", "c_pospro_1", "c_profess1dig_1", "Titoccup",
    "Propabit", "tipabitaz_new", "c_Superf", "c_Stanze", "Possauto",
    "Numauto_topcod", "Posspc", "Numpc_topcod", "Internet", "Risecon",
    "Sitecon", "povassc_rico", "poveri_rico", "povassc", "poveri",
    "rgn", "rip", div_cols
  ))
  needed <- intersect(needed, nms)
  dt <- fread_zip_member(zip_file, zip_member(zip_file), select = needed)
  dt[, weight := as_num(coalesce_col(.SD, c("w_anno_rico", "w_anno"), 1))]
  dt[!is.finite(weight) | weight <= 0, weight := 1]
  attr(dt, "division_cols") <- div_cols
  dt[, source_year := year]
  dt
}

target_bridge_division_shares <- function(target_years) {
  target_path <- file.path(
    "data-raw",
    "italy_hbs",
    "IT_income_targets_from_expense_bridge_2015_2020_level1.csv"
  )
  if (!file.exists(target_path)) {
    stop(
      "Missing bridge-corrected Italy income targets: ", target_path, "\n",
      "Run scripts/build_it_income_targets_from_expense_bridge.R first.",
      call. = FALSE
    )
  }
  dt <- fread(target_path)
  dt <- dt[year %in% target_years]
  dt[, coicop_div := sub("^CP", "", coicop)]
  dt[, target_share := adjusted_share_pm / 1000]
  dt[, .(year = as.integer(year), category, coicop_div, target_share)]
}

prepare_expenditure_matrix <- function(dt) {
  div_cols <- attr(dt, "division_cols")
  div_cols <- div_cols[div_cols %in% names(dt)]
  div_cols <- div_cols[
    grepl("^d_[0-9]{2}(_str)?_rico$", div_cols) |
      grepl("^d_[0-9]{2}(_str|_aggr_1)?$", div_cols)
  ]
  div_cols <- div_cols[!grepl("^d_13", div_cols)]
  div_codes <- substr(sub("^d_", "", sub("_rico$", "", div_cols)), 1L, 2L)
  keep <- !duplicated(div_codes)
  div_cols <- div_cols[keep]
  div_codes <- div_codes[keep]
  x <- as.matrix(dt[, ..div_cols])
  storage.mode(x) <- "numeric"
  x[!is.finite(x) | x < 0] <- 0
  colnames(x) <- div_codes
  x
}

shares_from_probabilities <- function(exp_mat, w, p) {
  out <- vector("list", ncol(p))
  for (g in seq_len(ncol(p))) {
    wg <- w * p[, g]
    cons <- colSums(exp_mat * wg, na.rm = TRUE)
    s <- cons / sum(cons)
    out[[g]] <- data.table(
      category = colnames(p)[g],
      coicop_div = colnames(exp_mat),
      share = as.numeric(s)
    )
  }
  rbindlist(out)
}

group_shares_from_probabilities <- function(exp_mat, w, p) {
  total_exp <- rowSums(exp_mat, na.rm = TRUE)
  group_total <- colSums(p * (w * total_exp), na.rm = TRUE)
  data.table(
    category = colnames(p),
    group_share = as.numeric(group_total / sum(group_total))
  )
}

target_group_shares_from_propensity <- function(year, dt, exp_mat, w) {
  ncomp <- as_num(coalesce_col(dt, c("c_Ncmp_altro", "ncomp", "NCOMP"), 1))
  ncomp[!is.finite(ncomp) | ncomp <= 0] <- 1
  total_exp <- rowSums(exp_mat, na.rm = TRUE)
  expense_quintile <- weighted_quintile(total_exp / carbonaro_scale(ncomp), w)

  expense_targets <- data.table(
    quintile = paste0("Q", expense_quintile),
    expenditure = total_exp * w
  )[
    !is.na(quintile) & is.finite(expenditure) & expenditure >= 0,
    .(expense_total = sum(expenditure, na.rm = TRUE)),
    by = quintile
  ]

  consumption_to_income <- load_consumption_to_income(
    "IT",
    start_year = year,
    end_year = year
  )
  income_categories <- c(
    "First quintile", "Second quintile", "Third quintile",
    "Fourth quintile", "Fifth quintile"
  )
  consumption_to_income <- consumption_to_income[category != "Total"]
  consumption_to_income[, quintile := paste0(
    "Q",
    match(category, income_categories)
  )]

  target <- merge(
    expense_targets,
    consumption_to_income[, .(quintile, category, consumption_to_income)],
    by = "quintile",
    all = FALSE
  )
  if (nrow(target) != 5L || any(!is.finite(target$consumption_to_income))) {
    stop("Could not build Italy income group-share target for ", year, call. = FALSE)
  }

  target[, adjusted_total := expense_total * 100 / consumption_to_income]
  target[, target_group_share := adjusted_total / sum(adjusted_total)]
  target[, .(category = quintile, target_group_share)]
}

objective_factory <- function(x, w, exp_mat, target, group_target, tau,
                              target_weight, group_target_weight, ridge) {
  categories <- paste0("Q", 1:5)
  target_q <- NULL
  if (!is.null(target) && target_weight > 0) {
    target_q <- copy(target)
    target_q[, category := paste0("Q", match(category, unique(category)))]
    target_q <- target_q[coicop_div %in% colnames(exp_mat)]
  }
  group_target_q <- copy(group_target)
  group_target_q <- group_target_q[category %in% categories]

  function(beta) {
    score <- as.numeric(x %*% beta)
    p <- quintile_probabilities(score, w, tau = tau)
    division_loss <- 0
    if (!is.null(target_q) && nrow(target_q) > 0L) {
      pred <- shares_from_probabilities(exp_mat, w, p)
      comp <- merge(
        pred,
        target_q,
        by = c("category", "coicop_div"),
        all = FALSE
      )
      division_loss <- mean((comp$share - comp$target_share)^2, na.rm = TRUE)
    }

    pred_group <- group_shares_from_probabilities(exp_mat, w, p)
    group_comp <- merge(
      pred_group,
      group_target_q,
      by = "category",
      all = FALSE
    )
    group_loss <- mean(
      (group_comp$group_share - group_comp$target_group_share)^2,
      na.rm = TRUE
    )

    target_weight * division_loss +
      group_target_weight * group_loss +
      ridge * mean(beta[-1L]^2)
  }
}

calibrate_year <- function(year, target, cfg) {
  message("Reading Istat HBS microdata ", year)
  target_year <- as.integer(year)
  if (!is.null(target)) {
    target <- target[year == target_year]
    if (nrow(target) == 0L) {
      stop("No bridge-corrected income target for ", year, call. = FALSE)
    }
  }
  dt <- read_istat_hbs_year(year, cfg$zips[as.character(year)])
  if (nrow(dt) > 2500L) {
    set.seed(cfg$seed + year)
    dt <- dt[sample.int(.N, 2500L)]
  }
  x <- build_household_features(dt)
  exp_mat <- prepare_expenditure_matrix(dt)
  w <- dt$weight
  group_target <- target_group_shares_from_propensity(target_year, dt, exp_mat, w)

  set.seed(cfg$seed + year)
  starts <- list(
    c(0, rep(0.15, ncol(x) - 1L))
  )
  obj <- objective_factory(
    x, w, exp_mat, target, group_target, cfg$tau, cfg$target_weight,
    cfg$group_target_weight, cfg$ridge
  )
  fits <- lapply(starts, function(start) {
    stats::optim(
      par = start,
      fn = obj,
      method = "BFGS",
      control = list(maxit = cfg$maxit, reltol = 1e-8),
      hessian = TRUE
    )
  })
  best <- fits[[which.min(vapply(fits, `[[`, numeric(1), "value"))]]
  se <- rep(NA_real_, length(best$par))
  if (!is.null(best$hessian)) {
    vcov <- tryCatch(solve(best$hessian), error = function(e) NULL)
    if (!is.null(vcov)) {
      se <- sqrt(abs(diag(vcov)))
    }
  }
  score <- as.numeric(x %*% best$par)
  p <- quintile_probabilities(score, w, tau = cfg$tau)
  shares <- shares_from_probabilities(exp_mat, w, p)
  shares[, year := year]
  group_shares <- group_shares_from_probabilities(exp_mat, w, p)
  group_shares <- merge(group_shares, group_target, by = "category", all.x = TRUE)
  group_shares[, year := year]
  probs <- as.data.table(p)
  probs[, household_row := .I]
  probs[, year := year]

  list(
    year = year,
    beta = best$par,
    se = se,
    beta_names = colnames(x),
    loss = best$value,
    shares = shares,
    group_shares = group_shares,
    probabilities = probs
  )
}

build_hbs_from_division_intensities <- function(calibrated, hbs_totals, categories, level) {
  calibrated_shares <- rbindlist(lapply(calibrated, `[[`, "shares"), use.names = TRUE)
  # Convert division shares into relative intensities around the all-households
  # division share of the same target year.
  total_div <- copy(hbs_totals)
  total_div[, coicop_div := substr(coicop, 1L, 2L)]
  total_div <- total_div[, .(
    total_div_consumption = sum(total_consumption, na.rm = TRUE)
  ), by = .(year, coicop_div)]
  total_div[, total_share := total_div_consumption / sum(total_div_consumption), by = year]

  intensities <- merge(
    calibrated_shares,
    total_div[, .(year, coicop_div, total_share)],
    by = c("year", "coicop_div"),
    all.x = TRUE
  )
  intensities[, intensity := share / total_share]

  dt <- merge(
    hbs_totals[, .(coicop, year, total_consumption)],
    intensities[, .(year, coicop_div, category, intensity)],
    by.x = c("year"),
    by.y = c("year"),
    allow.cartesian = TRUE
  )
  dt <- dt[substr(coicop, 1L, 2L) == coicop_div]
  dt[, consumption := total_consumption * intensity]
  dt <- dt[is.finite(consumption) & consumption > 0]
  dt[, `:=`(
    series_name = "Italy HBS income quintiles estimated from latent-income probabilities",
    category = factor(category, levels = paste0("Q", 1:5), labels = categories)
  )]
  dt[, category := as.character(category)]
  dt <- dt[, .(series_name, coicop, year = as.numeric(year), consumption, category)]

  dt_total <- hbs_totals[, .(
    series_name = "Italy HBS all-households total",
    coicop,
    year = as.numeric(year),
    total_consumption
  )]

  hbs(
    dt = dt,
    dt_total = dt_total,
    country = "IT",
    category = "income",
    categories = categories,
    level = level
  )
}

add_proxy_coicop_rows <- function(hbs_obj, mapping) {
  dt <- copy(hbs_obj$dt)
  dt_total <- copy(hbs_obj$dt_total)

  for (i in seq_len(nrow(mapping))) {
    child <- mapping$child[[i]]
    parent <- mapping$parent[[i]]

    if (!child %in% dt$coicop && parent %in% dt$coicop) {
      child_dt <- copy(dt[coicop == parent])
      child_dt[, coicop := child]
      if ("series_name" %in% names(child_dt)) {
        child_dt[, series_name := paste0(series_name, " (proxy from ", parent, ")")]
      } else {
        child_dt[, series_name := paste0("Italy HBS proxy from ", parent)]
      }
      dt <- rbindlist(list(dt, child_dt), use.names = TRUE, fill = TRUE)
    }

    if (!child %in% dt_total$coicop && parent %in% dt_total$coicop) {
      child_total <- copy(dt_total[coicop == parent])
      child_total[, coicop := child]
      if ("series_name" %in% names(child_total)) {
        child_total[, series_name := paste0(series_name, " (proxy from ", parent, ")")]
      } else {
        child_total[, series_name := paste0("Italy HBS total proxy from ", parent)]
      }
      dt_total <- rbindlist(list(dt_total, child_total), use.names = TRUE, fill = TRUE)
    }
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

message("Loading group-share calibration and all-households HBS totals")
target <- if (cfg$target_weight > 0) {
  target_bridge_division_shares(cfg$target_years)
} else {
  NULL
}
hbs_totals_src <- load_hbs(
  "IT", "income", level = cfg$level,
  start_year = cfg$calibration_year,
  end_year = max(cfg$target_years)
)
totals <- copy(hbs_totals_src$dt_total[
  year %in% cfg$target_years &
    is.finite(total_consumption) &
    total_consumption > 0
])

calibrated <- lapply(cfg$target_years, calibrate_year, target = target, cfg = cfg)
hbs_latent <- build_hbs_from_division_intensities(
  calibrated,
  totals,
  hbs_totals_src$categories,
  cfg$level
)
hbs_latent <- add_proxy_coicop_rows(
  hbs_latent,
  data.table(
    child = c("013", "023", "064", "074", "097", "098", "103", "122",
              "13", "131", "132", "133", "139"),
    parent = c("01", "02", "06", "07", "09", "09", "10", "12",
               "12", "12", "12", "12", "12")
  )
)

out_rds <- file.path(out_dir, "IT_income_hbs_latent_probabilities_2015_2020_level2.rds")
saveRDS(hbs_latent, out_rds)

fwrite(
  rbindlist(lapply(calibrated, function(x) {
    z <- x$beta / x$se
    p_value <- 2 * stats::pnorm(abs(z), lower.tail = FALSE)
    data.table(
      year = x$year,
      feature = x$beta_names,
      beta = x$beta,
      std_error = x$se,
      p_value = p_value,
      loss = x$loss
    )
  })),
  file.path(out_dir, "IT_income_hbs_latent_probability_coefficients.csv")
)
fwrite(
  rbindlist(lapply(calibrated, `[[`, "shares"), use.names = TRUE),
  file.path(out_dir, "IT_income_hbs_latent_probability_division_shares.csv")
)
fwrite(
  rbindlist(lapply(calibrated, `[[`, "group_shares"), use.names = TRUE),
  file.path(out_dir, "IT_income_hbs_latent_probability_group_shares.csv")
)
fwrite(
  hbs_latent$dt[, .(
    total_consumption = sum(consumption, na.rm = TRUE)
  ), by = .(year, category)],
  file.path(out_dir, "IT_income_hbs_latent_probability_diagnostics.csv")
)

message("Wrote: ", normalizePath(out_rds, winslash = "/", mustWork = FALSE))
message("Rows in estimated HBS dt: ", nrow(hbs_latent$dt))
message("Rows in estimated HBS dt_total: ", nrow(hbs_latent$dt_total))
