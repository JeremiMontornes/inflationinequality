
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

COICOP bridge HTML tables:

- [France HICP-HBS bridge, income level 2](https://jeremimontornes.github.io/inflationinequality/france_coicop_hicp_hbs_bridge_income_level2_2010_2026.html)
- [France HICP-HBS bridge audit, income level 2](https://jeremimontornes.github.io/inflationinequality/france_coicop_hicp_hbs_bridge_income_level2_2010_2026_audit.html)
- [France HICP-HBS bridge, INSEE income level 3](https://jeremimontornes.github.io/inflationinequality/france_coicop_hicp_hbs_bridge_income_level3_insee_2010_2026.html)
- [France HICP-HBS bridge audit, INSEE income level 3](https://jeremimontornes.github.io/inflationinequality/france_coicop_hicp_hbs_bridge_income_level3_insee_2010_2026_audit.html)
- [France ECOICOP v2 to COICOP v1 level 3 audit](https://jeremimontornes.github.io/inflationinequality/france_hicp_ecoicopv2_to_coicopv1_level3_audit.html)

## Example

Let us visualize inflation inequality across income quintiles in France since 2019, using the France level 3 COICOP workflow.

``` r
library(inflationinequality)
inflation <- calculate_inflation("FR", "income", start_year = 2019, level = 3,
                                 france_insee_income_groups = "quintile")
plot_time_series(inflation)
```

<img src="man/figures/README-example-1.svg" alt="" width="100%" />

Source: Eurostat and national statistical institutes, HICP-HBS. Note: year-on-year change

## Methodological metadata

The table below summarises the main methodological choices currently made by the package and identifies which choices users can override. It is intended as a living reference for handling country-specific cases.

| topic | package_default | user_parameter | user_can_change | notes |
|:---|:---|:---|:---|:---|
| Default population groups | income quintiles for category = 'income'; age groups for category = 'age'; rural/town/city for category = 'urban'. | category argument, or custom_hbs with a custom ordered categories vector. | Yes | The ordered categories vector controls which groups are treated as bottom and top. |
| COICOP level | COICOP 3 digits by default (package level = 2); COICOP 2 to 4 digits are accepted by load and calculation functions. | level argument. | Yes | Eurostat HBS income data are generally available at 3-digit COICOP (package level = 2), which is the package default. |
| HBS-to-CPI timing | Each CPI weight year is matched to the most recent available HBS wave at or before that year; earliest available HBS is used if no prior wave exists. | specific_hbs_year, interpolated_hbs, or custom_hbs/custom_index_weights. | Yes | This is one of the main places where country-specific metadata can improve defaults. |
| HBS interpolation | No interpolation by default. | interpolated_hbs = TRUE. | Yes | Useful when HBS waves are sparse and users want smoother weights over time. |
| Specific HBS wave | No single HBS wave is forced by default. | specific_hbs_year. | Yes | Useful for sensitivity analysis or reproducing a fixed-wave methodology. |
| Incomplete CPI coverage | Missing CPI series are not synthesised by default. | ensure_complete_cpi = TRUE. | Yes | Synthesised CPI data use parent-category price movements. |
| Inflation aggregation | Inflation is computed from COICOP contributions using annual HICP weights adjusted by HBS relative expenditure shares. | custom_cpi, custom_index_weights, custom_hbs, level, and date arguments. | Yes |  |

### Country-specific COICOP data caveats

| country | coicop_case | population_group_caveat | housing_caveat | package_status |
|:---|:---|:---|:---|:---|
| France | France income level = 3 uses bundled INSEE Budget de famille 2017 data at 4-digit COICOP. Eurostat HBS income data remain the default for harmonised level = 2 comparisons. | INSEE published income-decile IPC series are by 'niveau de vie': household disposable income divided by consumption units. This is not strictly the same grouping concept as the Eurostat HBS income-quintile tables used by load_hbs(). | Large housing-weight gaps can partly reflect the different income grouping concepts. In HICP calculations, CP042 imputed rents are not directly used because they are absent from the HICP basket; observed differences mainly affect CP041 actual rents and related housing items. | Implemented for level = 3 income calculations with france_insee_income_groups = 'decile' or 'quintile'. |
| Belgium | No Belgium-specific override is implemented yet; Eurostat harmonised HBS income categories are used if available. Belgium is the strongest candidate for a country-specific quartile rule. | Bruegel treats Belgium with bottom/top quartiles rather than quintiles. The package has no automatic Belgium quartile override yet. |  | Under development; use custom_hbs for national quartile workflows. |
| Italy | If the Eurostat 2010 income HBS wave is available, Italy uses that wave for all CPI weight years; other Eurostat HBS waves are not used by default. | Bruegel warns that I.Stat data used in earlier versions identify expenditure quintiles, not income quintiles; Italy was therefore removed from their final figures. |  | Partly implemented through the fixed 2010 Eurostat income HBS default; national-source level = 3 support is not integrated. |

## Vignettes

The introduction vignette walks through the package methodology, core functions, and plotting helpers:

- [Introduction to inflationinequality](https://jeremimontornes.github.io/inflationinequality/articles/inflationinequality-intro.html)
- [Advanced options](https://jeremimontornes.github.io/inflationinequality/articles/advanced-options.html)
- [Using national data](https://jeremimontornes.github.io/inflationinequality/articles/using-custom-data.html)
- [Calculating inflation burden](https://jeremimontornes.github.io/inflationinequality/articles/inflation-burden.html)
- [Verifying calculated inflation](https://jeremimontornes.github.io/inflationinequality/articles/verifying-calculated-inflation.html)
