suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
})

script_file <- if (sys.nframe() >= 1L && !is.null(sys.frame(1)$ofile)) {
  sys.frame(1)$ofile
} else {
  normalizePath("scripts/simulate_es_epf_2020_energy_policy_microdata.R", mustWork = TRUE)
}
root <- normalizePath(file.path(dirname(script_file), ".."), mustWork = TRUE)

zip_path <- Sys.getenv("ES_EPF_2020_ZIP", unset = "")
if (!nzchar(zip_path)) {
  zip_path <- file.path(Sys.getenv("TEMP"), "ine_epf_2020.zip")
}
if (!file.exists(zip_path)) {
  stop(
    "EPF 2020 ZIP not found. Download INE datos_2020.zip and set ",
    "ES_EPF_2020_ZIP, or place it at TEMP/ine_epf_2020.zip.",
    call. = FALSE
  )
}

cfg <- list(
  year = 2020L,
  electricity_coicop = "0451",
  price_shock = 0.30,
  elasticity = -0.2,
  reference_cost_billion = 20,
  household_electricity_share = 1 / 3
)

out_dir <- file.path(root, "docs", "energy_policy_microdata_es_epf_2020")
data_dir <- file.path(root, "data-raw", "spain_epf")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

work_dir <- file.path(tempdir(), "ine_epf_2020_micro_policy")
if (dir.exists(work_dir)) {
  unlink(work_dir, recursive = TRUE, force = TRUE)
}
dir.create(work_dir, recursive = TRUE)
utils::unzip(
  zip_path,
  files = c("EPFgastos_2020.zip", "EPFhogar_2020.zip"),
  exdir = work_dir
)
utils::unzip(file.path(work_dir, "EPFgastos_2020.zip"), exdir = file.path(work_dir, "EPFgastos_2020"))
utils::unzip(file.path(work_dir, "EPFhogar_2020.zip"), exdir = file.path(work_dir, "EPFhogar_2020"))

weighted_ntile <- function(x, w, n) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  out <- rep(NA_integer_, length(x))
  if (!any(ok)) {
    return(out)
  }
  ord <- order(x[ok], seq_along(x[ok]))
  ww <- w[ok][ord]
  rank <- (cumsum(ww) - 0.5 * ww) / sum(ww)
  out[which(ok)[ord]] <- pmin(n, pmax(1L, floor(n * rank) + 1L))
  out
}

weighted_sum <- function(x, w) {
  sum(x * w, na.rm = TRUE)
}

pct <- function(x, denom) {
  if (!is.finite(denom) || denom == 0) {
    return(NA_real_)
  }
  100 * x / denom
}

weighted_quantile <- function(x, w, probs = c(0.25, 0.5, 0.75)) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  if (!any(ok)) {
    return(rep(NA_real_, length(probs)))
  }
  x <- x[ok]
  w <- w[ok]
  ord <- order(x)
  x <- x[ord]
  w <- w[ord]
  cw <- cumsum(w) / sum(w)
  vapply(probs, function(p) x[which(cw >= p)[1L]], numeric(1))
}

households <- fread(
  file.path(work_dir, "EPFhogar_2020", "CSV", "EPFhogar_2020.csv"),
  sep = "\t",
  select = c("ANOENC", "NUMERO", "FACTOR", "UC1", "IMPEXAC", "GASTOT"),
  na.strings = c("", "NA")
)
households <- households[
  !is.na(NUMERO) &
    !is.na(FACTOR) & FACTOR > 0 &
    !is.na(UC1) & UC1 > 0 &
    !is.na(IMPEXAC) & IMPEXAC > 0 &
    !is.na(GASTOT) & GASTOT > 0
]
households[, `:=`(
  weight = FACTOR,
  equivalised_income = IMPEXAC / UC1,
  total_consumption = GASTOT / 1000
)]
households[, `:=`(
  decile_id = weighted_ntile(equivalised_income, weight, 10L),
  centile_id = weighted_ntile(equivalised_income, weight, 100L)
)]
households[, `:=`(
  decile = paste0("Decile ", decile_id),
  centile = paste0("Centile ", centile_id),
  below_median = decile_id <= 5L
)]

expenses <- fread(
  file.path(work_dir, "EPFgastos_2020", "CSV", "EPFgastos_2020.csv"),
  sep = "\t",
  select = c("NUMERO", "CODIGO", "GASTO"),
  na.strings = c("", "NA")
)
expenses <- expenses[
  NUMERO %in% households$NUMERO &
    !is.na(CODIGO) &
    !is.na(GASTO) & GASTO >= 0
]
expenses[, coicop := substr(CODIGO, 1L, 4L)]
electricity <- expenses[
  coicop == cfg$electricity_coicop,
  .(electricity_expenditure = sum(GASTO / 1000, na.rm = TRUE)),
  by = NUMERO
]

dt <- merge(
  households[, .(
    household_id = NUMERO,
    year = ANOENC,
    weight,
    equivalised_income,
    decile_id,
    decile,
    centile_id,
    centile,
    below_median,
    total_consumption
  )],
  electricity,
  by.x = "household_id",
  by.y = "NUMERO",
  all.x = TRUE
)
dt[is.na(electricity_expenditure), electricity_expenditure := 0]
dt <- dt[total_consumption > 0]
dt[, `:=`(
  electricity_share = electricity_expenditure / total_consumption,
  equivalent_variation = cfg$price_shock * electricity_expenditure / total_consumption,
  equivalent_variation_eur = cfg$price_shock * electricity_expenditure
)]

build_rows <- function(base, scenario, label, eligible, policy_type, transfer) {
  out <- copy(base)
  out[, `:=`(
    scenario = scenario,
    policy_label = label,
    policy_type = policy_type,
    eligible = eligible,
    transfer_eur = transfer
  )]
  out
}

need <- dt$equivalent_variation_eur
weighted_mean_all <- weighted.mean(need, dt$weight)
eligible <- dt$below_median
weighted_mean_eligible <- weighted.mean(need[eligible], dt$weight[eligible])

policies <- rbindlist(list(
  build_rows(dt, "A1", "Prix regule", TRUE, "price_regulation", need),
  build_rows(dt, "B1", "Transfert forfaitaire", TRUE, "lump_sum", rep(weighted_mean_all, nrow(dt))),
  build_rows(dt, "C1", "Transfert en fonction de la consommation passee", TRUE, "past_consumption", need),
  build_rows(dt, "A2", "Prix regule", eligible, "price_regulation", fifelse(eligible, need, 0)),
  build_rows(dt, "B2", "Transfert forfaitaire", eligible, "lump_sum", fifelse(eligible, weighted_mean_eligible, 0)),
  build_rows(dt, "C2", "Transfert en fonction de la consommation passee", eligible, "past_consumption", fifelse(eligible, need, 0))
), use.names = TRUE)
policies[, `:=`(
  unmet_need = pmax(equivalent_variation_eur - transfer_eur, 0),
  excess_transfer = pmax(transfer_eur - equivalent_variation_eur, 0),
  net_compensation = transfer_eur - equivalent_variation_eur
)]

build_policy_table <- function(scope) {
  scenario_ids <- if (identical(scope, "all")) c("A1", "B1", "C1") else c("A2", "B2", "C2")
  eval_dt <- policies[scenario %in% scenario_ids]
  if (identical(scope, "below_median")) {
    eval_dt <- eval_dt[below_median == TRUE]
  }
  all_need <- policies[scenario == "A1", weighted_sum(equivalent_variation_eur, weight)]
  eval_dt[, {
    total_transfer <- weighted_sum(transfer_eur, weight)
    total_need_scope <- weighted_sum(equivalent_variation_eur, weight)
    over <- transfer_eur > equivalent_variation_eur
    under <- transfer_eur < equivalent_variation_eur
    eligible_need_share <- total_need_scope / all_need
    demand_response_household <- if (policy_type[1] == "price_regulation") {
      -cfg$elasticity * cfg$price_shock * eligible_need_share
    } else {
      0
    }
    .(
      policy_label = policy_label[1],
      aggregate_consumption_change_pct = 100 * cfg$household_electricity_share * demand_response_household,
      household_consumption_change_pct = 100 * demand_response_household,
      total_cost_billion = cfg$reference_cost_billion * total_transfer / all_need,
      public_spending_overcompensation_pct = pct(weighted_sum(excess_transfer, weight), total_transfer),
      public_spending_top_10_pct = if (identical(scope, "below_median")) 0 else {
        pct(weighted_sum(transfer_eur[decile_id == 10L], weight[decile_id == 10L]), total_transfer)
      },
      households_overcompensated_pct = pct(weighted_sum(as.numeric(over), weight), sum(weight)),
      mean_overcompensation_consumption_pct = if (any(over)) {
        weighted.mean(100 * excess_transfer[over] / total_consumption[over], weight[over])
      } else {
        0
      },
      households_undercompensated_pct = pct(weighted_sum(as.numeric(under), weight), sum(weight)),
      mean_undercompensation_consumption_pct = if (any(under)) {
        weighted.mean(100 * unmet_need[under] / total_consumption[under], weight[under])
      } else {
        0
      }
    )
  }, by = scenario][order(scenario)]
}

tables <- list(
  all_households = build_policy_table("all"),
  below_median = build_policy_table("below_median")
)

centile_stats <- dt[, .(
  electricity_share_mean = weighted.mean(electricity_share, weight),
  electricity_share_p25 = weighted_quantile(electricity_share, weight, 0.25),
  electricity_share_p50 = weighted_quantile(electricity_share, weight, 0.50),
  electricity_share_p75 = weighted_quantile(electricity_share, weight, 0.75),
  transfer_mean = weighted.mean(equivalent_variation_eur, weight)
), by = centile_id][order(centile_id)]

decile_stats <- dt[, {
  qs_share <- weighted_quantile(electricity_share, weight, c(0.25, 0.5, 0.75))
  qs_transfer <- weighted_quantile(equivalent_variation_eur, weight, c(0.25, 0.5, 0.75))
  .(
    electricity_share_mean = weighted.mean(electricity_share, weight),
    electricity_share_p25 = qs_share[1],
    electricity_share_p50 = qs_share[2],
    electricity_share_p75 = qs_share[3],
    transfer_mean = weighted.mean(equivalent_variation_eur, weight),
    transfer_p25 = qs_transfer[1],
    transfer_p50 = qs_transfer[2],
    transfer_p75 = qs_transfer[3]
  )
}, by = .(decile_id, decile)][order(decile_id)]

figure6 <- ggplot(centile_stats, aes(x = centile_id, y = 100 * electricity_share_mean)) +
  geom_line(color = "#2f5d8c", linewidth = 0.7) +
  geom_point(color = "#2f5d8c", size = 1.6) +
  labs(
    title = "Depenses d'electricite en fonction du niveau de vie",
    subtitle = "INE EPF 2020 microdata, chaque point represente 1% pondere des menages",
    x = "Centile de revenu equivalise",
    y = "Part dans la depense totale (%)"
  ) +
  theme_minimal(base_size = 11)

figure7 <- ggplot(decile_stats, aes(x = decile_id, y = 100 * electricity_share_p50)) +
  geom_ribbon(aes(ymin = 100 * electricity_share_p25, ymax = 100 * electricity_share_p75),
              fill = "grey70", alpha = 0.45) +
  geom_line(color = "#2f5d8c", linewidth = 0.7) +
  geom_point(color = "#2f5d8c", size = 2) +
  scale_x_continuous(breaks = 1:10, labels = paste0("D", 1:10)) +
  labs(
    title = "Heterogeneite intra-decile des depenses d'electricite",
    subtitle = "Mediane et intervalle interquartile par decile",
    x = NULL,
    y = "Part dans la depense totale (%)"
  ) +
  theme_minimal(base_size = 11)

transfer_centile <- policies[scenario %in% c("A1", "C1"),
  .(transfer_mean = weighted.mean(transfer_eur, weight)),
  by = .(scenario, centile_id)
][order(scenario, centile_id)]

figure8 <- ggplot(transfer_centile, aes(x = centile_id, y = transfer_mean, color = scenario)) +
  geom_line(linewidth = 0.7) +
  labs(
    title = "Montants des transferts par niveau de vie",
    subtitle = "Scenarios A.1 et C.1, INE EPF 2020 microdata",
    x = "Centile de revenu equivalise",
    y = "Transfert moyen par menage",
    color = NULL
  ) +
  theme_minimal(base_size = 11)

figure9 <- ggplot(decile_stats, aes(x = decile_id, y = transfer_p50)) +
  geom_ribbon(aes(ymin = transfer_p25, ymax = transfer_p75), fill = "grey70", alpha = 0.45) +
  geom_line(color = "#2f5d8c", linewidth = 0.7) +
  geom_point(color = "#2f5d8c", size = 2) +
  scale_x_continuous(breaks = 1:10, labels = paste0("D", 1:10)) +
  labs(
    title = "Heterogeneite intra-decile des transferts",
    subtitle = "Mediane et intervalle interquartile, scenarios A.1 et C.1",
    x = NULL,
    y = "Transfert par menage"
  ) +
  theme_minimal(base_size = 11)

plots <- list(figure6 = figure6, figure7 = figure7, figure8 = figure8, figure9 = figure9)

fwrite(tables$all_households, file.path(out_dir, "ES_micro_table_all_households.csv"))
fwrite(tables$below_median, file.path(out_dir, "ES_micro_table_below_median.csv"))
fwrite(decile_stats, file.path(out_dir, "ES_micro_decile_stats.csv"))
fwrite(centile_stats, file.path(out_dir, "ES_micro_centile_stats.csv"))
for (nm in names(plots)) {
  ggsave(file.path(out_dir, paste0("ES_micro_", nm, ".png")), plots[[nm]], width = 8, height = 5, dpi = 150)
}

out_rds <- file.path(data_dir, "ES_epf_2020_energy_policy_microdata_results.rds")
saveRDS(
  list(
    household_data = dt,
    household_policies = policies,
    tables = tables,
    decile_stats = decile_stats,
    centile_stats = centile_stats,
    config = cfg
  ),
  out_rds,
  compress = "xz"
)

diagnostics <- data.table(
  source = "INE EPF 2020 microdata, author calculations",
  n_households = nrow(dt),
  weighted_households = sum(dt$weight),
  households_with_electricity = dt[electricity_expenditure > 0, .N],
  weighted_households_with_electricity_pct = pct(
    dt[electricity_expenditure > 0, sum(weight)],
    dt[, sum(weight)]
  ),
  mean_electricity_expenditure = weighted.mean(dt$electricity_expenditure, dt$weight),
  mean_total_consumption = weighted.mean(dt$total_consumption, dt$weight),
  mean_equivalent_variation_eur = weighted.mean(dt$equivalent_variation_eur, dt$weight),
  output_rds = out_rds
)
fwrite(diagnostics, file.path(out_dir, "ES_micro_diagnostics.csv"))

message("Wrote: ", normalizePath(out_dir, winslash = "/", mustWork = TRUE))
message("Wrote: ", normalizePath(out_rds, winslash = "/", mustWork = TRUE))
message("All households")
print(tables$all_households)
message("Below median")
print(tables$below_median)
