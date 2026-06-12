suppressPackageStartupMessages({
  library(data.table)
  library(inflationinequality)
})

out_raw <- file.path("data-raw", "group_consumption_shares.csv")
out_ext <- file.path("inst", "extdata", "group_consumption_shares.csv")
out_income_raw <- file.path("data-raw", "income_group_consumption_shares.csv")
out_income_ext <- file.path("inst", "extdata", "income_group_consumption_shares.csv")
dir.create(dirname(out_raw), recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(out_ext), recursive = TRUE, showWarnings = FALSE)
category_data <- inflationinequality:::category_data

build_income_shares <- function() {
  categories <- category_data$income$categories
  old_names <- paste0("QU", 1:5)

  message("Loading Eurostat hbs_exp_t133 mean consumption expenditure")
  dt <- inflationinequality:::eurostat_json_data(
    "hbs_exp_t133",
    filters = list(freq = "A", unit = "PPS_HH")
  )
  dt <- as.data.table(dt)

  shares <- dt[
    quant_inc %in% old_names & is.finite(values),
    .(
      hbs_category = "income",
      country = as.character(geo),
      year = as.integer(substr(as.character(time), 1L, 4L)),
      category = categories[match(quant_inc, old_names)],
      mean_expenditure_pps_hh = as.numeric(values),
      household_count = NA_real_,
      household_share = 1 / length(categories)
    )
  ]
  shares <- shares[
    ,
    .SD[uniqueN(category) == length(categories)],
    by = .(hbs_category, country, year)
  ]
  shares[
    ,
    `:=`(
      group_consumption_share = mean_expenditure_pps_hh / sum(mean_expenditure_pps_hh),
      group_consumption_share_pm = 1000 * mean_expenditure_pps_hh / sum(mean_expenditure_pps_hh),
      source = "Eurostat hbs_exp_t133 PPS_HH; equal household income quintiles"
    ),
    by = .(hbs_category, country, year)
  ]

  italy_path <- file.path(
    "data-raw", "italy_hbs",
    "IT_income_group_shares_from_expense_bridge_2015_2020.csv"
  )
  if (file.exists(italy_path)) {
    message("Overriding Italy 2015/2020 with bridge-corrected estimates")
    it <- fread(italy_path)
    it <- it[
      Group %in% categories,
      .(
        hbs_category = "income",
        country = "IT",
        year = as.integer(year),
        category = as.character(Group),
        mean_expenditure_pps_hh = NA_real_,
        household_count = NA_real_,
        household_share = 1 / length(categories),
        group_consumption_share = as.numeric(share_total) / 100,
        group_consumption_share_pm = 10 * as.numeric(share_total),
        source = "Istat HBS bridge-corrected income/expenditure shares"
      )
    ]
    shares <- shares[!(country == "IT" & year %in% unique(it$year))]
    shares <- rbindlist(list(shares, it), use.names = TRUE)
  }

  shares[]
}

build_size_weighted_shares <- function(hbs_category, exp_dataset, car_dataset,
                                       category_col, exp_old_names, categories) {
  message("Loading Eurostat ", exp_dataset, " mean consumption expenditure")
  exp <- inflationinequality:::eurostat_json_data(
    exp_dataset,
    filters = list(freq = "A", unit = "PPS_HH")
  )
  exp <- as.data.table(exp)

  message("Loading Eurostat ", car_dataset, " household counts")
  car <- inflationinequality:::eurostat_json_data(
    car_dataset,
    filters = list(freq = "A", hhcaract = "NR")
  )
  car <- as.data.table(car)

  exp <- exp[
    get(category_col) %in% exp_old_names & is.finite(values),
    .(
      country = as.character(geo),
      year = as.integer(substr(as.character(time), 1L, 4L)),
      category = categories[match(get(category_col), exp_old_names)],
      mean_expenditure_pps_hh = as.numeric(values)
    )
  ]
  car <- car[
    get(category_col) %in% exp_old_names & is.finite(values),
    .(
      country = as.character(geo),
      year = as.integer(substr(as.character(time), 1L, 4L)),
      category = categories[match(get(category_col), exp_old_names)],
      household_count = as.numeric(values)
    )
  ]

  shares <- merge(exp, car, by = c("country", "year", "category"))
  shares <- shares[
    ,
    .SD[uniqueN(category) == length(categories)],
    by = .(country, year)
  ]
  shares[
    ,
    `:=`(
      hbs_category = hbs_category,
      household_share = household_count / sum(household_count),
      group_total_expenditure = mean_expenditure_pps_hh * household_count
    ),
    by = .(country, year)
  ]
  shares[
    ,
    `:=`(
      group_consumption_share = group_total_expenditure / sum(group_total_expenditure),
      group_consumption_share_pm = 1000 * group_total_expenditure / sum(group_total_expenditure),
      source = paste0("Eurostat ", exp_dataset, " PPS_HH x ", car_dataset, " NR")
    ),
    by = .(country, year)
  ]
  shares[, group_total_expenditure := NULL]
  shares[]
}

build_france_insee_2017_shares <- function() {
  files <- list(
    income = file.path("inst", "extdata", "INSEE_HBS_2017_level3.RDS"),
    age = file.path("inst", "extdata", "INSEE_HBS_2017_age_level3.RDS"),
    urban = file.path("inst", "extdata", "INSEE_HBS_2017_urban_level3.RDS")
  )
  files <- files[file.exists(unlist(files))]
  rbindlist(lapply(names(files), function(hbs_category) {
    hbs_obj <- readRDS(files[[hbs_category]])
    dt <- as.data.table(hbs_obj$dt)
    out <- dt[
      nchar(coicop) == 2L,
      .(mean_expenditure_pps_hh = sum(consumption, na.rm = TRUE)),
      by = category
    ]
    out <- out[category %in% hbs_obj$categories]
    out[, category := factor(category, levels = hbs_obj$categories)]
    setorder(out, category)
    out[, `:=`(
      hbs_category = hbs_category,
      country = "FR",
      year = 2017L,
      household_count = NA_real_,
      household_share = NA_real_,
      group_consumption_share = mean_expenditure_pps_hh / sum(mean_expenditure_pps_hh),
      group_consumption_share_pm = 1000 * mean_expenditure_pps_hh / sum(mean_expenditure_pps_hh),
      source = "Insee Les depenses des menages en 2017 TF101/TF102/TF106"
    )]
    out[, category := as.character(category)]
    out
  }), use.names = TRUE, fill = TRUE)
}

shares <- rbindlist(
  list(
    build_income_shares(),
    build_size_weighted_shares(
      hbs_category = "age",
      exp_dataset = "hbs_exp_t135",
      car_dataset = "hbs_car_t314",
      category_col = "age",
      exp_old_names = category_data$age$old_names,
      categories = category_data$age$categories
    ),
    build_size_weighted_shares(
      hbs_category = "urban",
      exp_dataset = "hbs_exp_t136",
      car_dataset = "hbs_car_t315",
      category_col = "deg_urb",
      exp_old_names = category_data$urban$old_names,
      categories = category_data$urban$categories
    )
  ),
  use.names = TRUE,
  fill = TRUE
)

france_insee_2017 <- build_france_insee_2017_shares()
shares <- shares[!(country == "FR" & year == 2017L &
                     hbs_category %in% france_insee_2017$hbs_category)]
shares <- rbindlist(list(shares, france_insee_2017), use.names = TRUE, fill = TRUE)

category_order <- rbindlist(lapply(names(category_data), function(hbs_category) {
  data.table(
    hbs_category = hbs_category,
    category = category_data[[hbs_category]]$categories,
    category_order = seq_along(category_data[[hbs_category]]$categories)
  )
}))
shares <- category_order[shares, on = .(hbs_category, category)]
setorder(shares, hbs_category, country, year, category_order)
shares[, category_order := NULL]

check <- shares[
  ,
  .(
    n_categories = uniqueN(category),
    sum_share = sum(group_consumption_share)
  ),
  by = .(hbs_category, country, year)
]
expected <- data.table(
  hbs_category = names(category_data),
  expected_categories = vapply(category_data, function(x) length(x$categories), integer(1L))
)
check <- expected[check, on = "hbs_category"]
bad <- check[n_categories != expected_categories | abs(sum_share - 1) > 1e-8]
if (nrow(bad) > 0L) {
  stop("Invalid group consumption shares:\n",
       paste(capture.output(print(bad)), collapse = "\n"),
       call. = FALSE)
}

fwrite(shares, out_raw)
fwrite(shares, out_ext)

income_compat <- shares[
  hbs_category == "income",
  .(
    country, year, category, mean_expenditure_pps_hh,
    group_consumption_share, group_consumption_share_pm, source
  )
]
fwrite(income_compat, out_income_raw)
fwrite(income_compat, out_income_ext)

message("Wrote: ", normalizePath(out_raw, winslash = "/", mustWork = FALSE))
message("Wrote: ", normalizePath(out_ext, winslash = "/", mustWork = FALSE))
message("Wrote compatibility income table: ",
        normalizePath(out_income_raw, winslash = "/", mustWork = FALSE))
message("Wrote compatibility income table: ",
        normalizePath(out_income_ext, winslash = "/", mustWork = FALSE))
