
<!-- README.md is generated from README.Rmd. Please edit that file -->

# inflationinequality

<!-- badges: start -->

[![R-CMD-check](https://github.com/jeremimontornes/inflationinequality/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jeremimontornes/inflationinequality/actions/workflows/R-CMD-check.yaml) <!-- badges: end -->

`inflationinequality` provides methods to calculate and visualize inflation inequality indicators.

## Features

- Calculate and visualize inflation and contributions to inflation by households categories
- Simulate counterfactual price indices

## Vignettes

The introduction vignette walks through the package methodology, core functions, and plotting helpers:

- [Introduction to inflationinequality](vignettes/inflationinequality-intro.Rmd)

After installing the package with vignettes, open it in R with:

``` r
vignette("inflationinequality-intro", package = "inflationinequality")
```

## Example

Let us visualize inflation inequality across income quintiles in Italy since 2019.

``` r
library(inflationinequality)
inflation <- calculate_inflation("IT", "income", start_year = 2019)
plot_time_series(inflation)
```

<img src="man/figures/README-example-1.svg" alt="" width="100%" />

## Methodological metadata

The table below summarises the main methodological choices currently made by the package and identifies which choices users can override. It is intended as a living reference for handling country-specific cases.

| topic | package_default | user_parameter | user_can_change | notes |
|:---|:---|:---|:---|:---|
| Price data source | Eurostat HICP via the hicp package, using the current ECOICOP v2 monthly index dataset when available. | custom_cpi in calculate_inflation() / calculate_contributions(); load_cpi() date and level arguments. | Yes | The resolver currently prefers ECOICOP v2 datasets with fallbacks for backend naming differences and legacy datasets. |
| HBS source | Eurostat HBS via DBnomics for harmonised income, age, and urban/rural breakdowns. | custom_hbs in calculate_weights(), calculate_inflation(), and calculate_contributions(). | Yes | National-source pipelines can be added as country-specific custom data workflows. |
| Default population groups | income quintiles for category = 'income'; age groups for category = 'age'; rural/town/city for category = 'urban'. | category argument, or custom_hbs with a custom ordered categories vector. | Yes | The ordered categories vector controls which groups are treated as bottom and top. |
| COICOP level | COICOP level 2 by default; levels 1 to 3 are accepted by load and calculation functions. | level argument. | Yes | Eurostat HBS is generally available only up to level 2; custom national data can go deeper. |
| Imputed rents | CP042 is not used when absent from CPI/HBS joins; no dedicated imputed-rent switch is currently exposed. | custom_cpi and custom_hbs; no dedicated CP042 argument yet. | Partly | A future explicit exclude_coicop argument would make this choice clearer. |
| HBS-to-CPI timing | Each CPI weight year is matched to the most recent available HBS wave at or before that year; earliest available HBS is used if no prior wave exists. | specific_hbs_year, interpolated_hbs, or custom_hbs/custom_index_weights. | Yes | This is one of the main places where country-specific metadata can improve defaults. |
| HBS interpolation | No interpolation by default. | interpolated_hbs = TRUE. | Yes | Useful when HBS waves are sparse and users want smoother weights over time. |
| Specific HBS wave | No single HBS wave is forced by default. | specific_hbs_year. | Yes | Useful for sensitivity analysis or reproducing a fixed-wave methodology. |
| Incomplete CPI coverage | Missing CPI series are not synthesised by default. | ensure_complete_cpi = TRUE. | Yes | Synthesised CPI data use parent-category price movements. |
| Inflation aggregation | Inflation is computed from COICOP contributions using annual HICP weights adjusted by HBS relative expenditure shares. | custom_cpi, custom_index_weights, custom_hbs, level, and date arguments. | Yes | This differs from the simpler Bruegel-style direct weighting of category inflation rates. |
| Official HICP comparison | plot_time_series() shows bottom, top, and package-computed total inflation; it does not yet add official HICP as a separate line. | Not currently parameterised. | No | Adding an official HICP overlay would improve validation plots. |
| Belgium (BE) | No Belgium-specific override yet; Eurostat harmonised HBS income categories are used if available. | custom_hbs; future country metadata could set quartile defaults. | Partly | Belgium is the strongest candidate for a country-specific quartile rule. |
| Italy (IT) | No Italy-specific override yet; Eurostat harmonised HBS is used if available. | custom_hbs; future country metadata should warn when source uses expenditure rather than income quintiles. | Partly | Italy should probably trigger a methodological warning unless the user supplies valid income-quintile HBS data. |
