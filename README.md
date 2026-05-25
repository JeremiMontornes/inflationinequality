
<!-- README.md is generated from README.Rmd. Please edit that file -->

# inflationinequality

<!-- badges: start -->

[![R-CMD-check](https://github.com/jeremimontornes/inflationinequality/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jeremimontornes/inflationinequality/actions/workflows/R-CMD-check.yaml) <!-- badges: end -->

`inflationinequality` provides methods to calculate and visualize inflation inequality indicators.

Package version: `0.0.2`

## Features

- Calculate and visualize inflation and contributions to inflation by households categories
- Calculate inflation burden as a share of disposable income
- Simulate counterfactual price indices

## Methodological workflow

The main workflow is:

`load_*()` -\> `calculate_weights()` -\> `calculate_price_indices()` / `calculate_inflation()` -\> `calculate_contributions()` -\> plots / validation.

## Vignettes

The introduction vignette walks through the package methodology, core functions, and plotting helpers:

- [Introduction to inflationinequality](vignettes/inflationinequality-intro.Rmd)
- [Advanced options](vignettes/advanced-options.Rmd)
- [Calculating inflation burden](vignettes/inflation-burden.Rmd)
- [Verifying calculated inflation](vignettes/verifying-calculated-inflation.Rmd)

## R workflow structure

``` text
R/load_data.R                         # Download HICP prices, HICP weights, and Eurostat HBS data
├── load_cpi()                        # Monthly HICP price indices
├── load_index_weights()              # Annual HICP item weights
└── load_hbs()                        # HBS expenditure shares by household category

R/calculate_weights.R                 # Match HICP weights with HBS shares
└── calculate_weights()               # Category-specific COICOP weights

R/calculate_contributions.R           # Compute COICOP contributions to inflation
└── calculate_contributions()

R/calculate_inflation.R               # Aggregate contributions into inflation indicators
├── calculate_inflation()
├── calculate_total_inflation()
└── calculate_inflation_gap()

R/calculate_price_indices.R           # Build chained HICP price indices in level
└── calculate_price_indices()         # Uses INSEE HBS level 3 automatically for France income level 3

R/calculate_inflation_burden.R        # Inflation cost burden by household group
├── load_consumption_to_income()
├── load_consumption_expenditure()
└── calculate_inflation_burden()

R/compare_to_official_hicp.R          # Validation against published all-items HICP
└── compare_to_official_hicp()

R/plot_inflation.R                    # Standard plots
├── plot_time_series()
├── plot_grouped_bar()
├── plot_weight_shares()
├── plot_group_price_indices()
├── plot_inflation_burden()
├── plot_contribution_gap()
└── plot_inflation_gap()

R/simulate.R                          # Counterfactual CPI scenarios
└── simulate_cpi()

R/coicop_bridge.R                     # COICOP matching and bridge tables
├── build_coicop_bridge()
└── write_coicop_bridge_html()
```

COICOP bridge HTML tables:

- [France HICP-HBS bridge, income level 2](https://jeremimontornes.github.io/inflationinequality/france_coicop_hicp_hbs_bridge_income_level2_2010_2026.html)
- [France HICP-HBS bridge audit, income level 2](https://jeremimontornes.github.io/inflationinequality/france_coicop_hicp_hbs_bridge_income_level2_2010_2026_audit.html)
- [France HICP-HBS bridge, INSEE income level 3](https://jeremimontornes.github.io/inflationinequality/france_coicop_hicp_hbs_bridge_income_level3_insee_2010_2026.html)
- [France HICP-HBS bridge audit, INSEE income level 3](https://jeremimontornes.github.io/inflationinequality/france_coicop_hicp_hbs_bridge_income_level3_insee_2010_2026_audit.html)
- [France ECOICOP v2 to COICOP v1 level 3 audit](https://jeremimontornes.github.io/inflationinequality/france_hicp_ecoicopv2_to_coicopv1_level3_audit.html)

## Example

Let us visualize inflation inequality across income quintiles in France since 2019.

``` r
library(inflationinequality)
inflation <- calculate_inflation("FR", "income", start_year = 2019, level = 2)
plot_time_series(inflation)
```

<img src="man/figures/README-example-1.svg" alt="" width="100%" />

Source: Eurostat and national statistical institutes, HICP-HBS. Note: year-on-year change

## Methodological metadata

The table below summarises the main methodological choices currently made by the package and identifies which choices users can override. It is intended as a living reference for handling country-specific cases.

| topic | package_default | user_parameter | user_can_change | notes |
|:---|:---|:---|:---|:---|
| Default population groups | income quintiles for category = 'income'; age groups for category = 'age'; rural/town/city for category = 'urban'. | category argument, or custom_hbs with a custom ordered categories vector. | Yes | The ordered categories vector controls which groups are treated as bottom and top. |
| COICOP level | COICOP 3 digits by default (package level = 2); COICOP 2 to 4 digits are accepted by load and calculation functions. | level argument. | Yes | Eurostat HBS income data are generally available at 3-digit COICOP (package level = 2), which is the package default. France income level = 3 uses bundled INSEE 4-digit COICOP data. Belgium and Italy national-source level 3 support is under development, following the country-specific approach used in Bruegel's work. A live Eurostat check found that level = 3 returns the same income HBS COICOP code set as level = 2 for DE, ES, and BE; IT currently exposes some 4-digit rows in DBnomics, but no national Italy level 3 source is integrated by default. |
| HBS-to-CPI timing | Each CPI weight year is matched to the most recent available HBS wave at or before that year; earliest available HBS is used if no prior wave exists. | specific_hbs_year, interpolated_hbs, or custom_hbs/custom_index_weights. | Yes | This is one of the main places where country-specific metadata can improve defaults. |
| HBS interpolation | No interpolation by default. | interpolated_hbs = TRUE. | Yes | Useful when HBS waves are sparse and users want smoother weights over time. |
| Specific HBS wave | No single HBS wave is forced by default. | specific_hbs_year. | Yes | Useful for sensitivity analysis or reproducing a fixed-wave methodology. |
| Incomplete CPI coverage | Missing CPI series are not synthesised by default. | ensure_complete_cpi = TRUE. | Yes | Synthesised CPI data use parent-category price movements. |
| Inflation aggregation | Inflation is computed from COICOP contributions using annual HICP weights adjusted by HBS relative expenditure shares. | custom_cpi, custom_index_weights, custom_hbs, level, and date arguments. | Yes |  |
### Country-specific COICOP data caveats

| country | iso2 | coicop_case | population_group_caveat | housing_caveat | package_status |
|:---|:---|:---|:---|:---|:---|
| France | FR | France income level = 3 uses bundled INSEE Budget de famille 2017 data at 4-digit COICOP. Eurostat HBS income data remain the default for harmonised level = 2 comparisons. | INSEE published income-decile IPC series are by 'niveau de vie': household disposable income divided by consumption units. This is not strictly the same grouping concept as the Eurostat HBS income-quintile tables used by load_hbs(). | Large housing-weight gaps can partly reflect the different income grouping concepts. In HICP calculations, CP042 imputed rents are not directly used because they are absent from the HICP basket; observed differences mainly affect CP041 actual rents and related housing items. | Implemented for level = 3 income calculations with france_insee_income_groups = 'decile' or 'quintile'. |
| Belgium | BE | No Belgium-specific override is implemented yet; Eurostat harmonised HBS income categories are used if available. Belgium is the strongest candidate for a country-specific quartile rule. | Bruegel treats Belgium with bottom/top quartiles rather than quintiles. The package has no automatic Belgium quartile override yet. |  | Under development; use custom_hbs for national quartile workflows. |
| Italy | IT | If the Eurostat 2010 income HBS wave is available, Italy uses that wave for all CPI weight years; other Eurostat HBS waves are not used by default. | Bruegel warns that I.Stat data used in earlier versions identify expenditure quintiles, not income quintiles; Italy was therefore removed from their final figures. |  | Partly implemented through the fixed 2010 Eurostat income HBS default; national-source level = 3 support is not integrated. |
