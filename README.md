
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
- [Online introduction page](https://jeremimontornes.github.io/inflationinequality/)

## Example

Let us visualize inflation inequality across income quintiles in Italy since 2019.

``` r
library(inflationinequality)
inflation <- calculate_inflation("IT", "income", start_year = 2019)
plot_time_series(inflation)
```

<img src="man/figures/README-example-1.svg" alt="" width="100%" />

Source: Eurostat and national statistical institutes, HICP-HBS. Note: year-on-year change

## Methodological metadata

The table below summarises the main methodological choices currently made by the package and identifies which choices users can override. It is intended as a living reference for handling country-specific cases.

| topic | package_default | user_parameter | user_can_change | notes |
|:---|:---|:---|:---|:---|
| Default population groups | income quintiles for category = 'income'; age groups for category = 'age'; rural/town/city for category = 'urban'. | category argument, or custom_hbs with a custom ordered categories vector. | Yes | The ordered categories vector controls which groups are treated as bottom and top. |
| COICOP level | COICOP digits 2 by default; digits 2 to 4 are accepted by load and calculation functions. | level argument. | Yes | Eurostat HBS is generally available up to level 2 (3-digit COICOP). National databases can support level 3 (4-digit COICOP). |
| HBS-to-CPI timing | Each CPI weight year is matched to the most recent available HBS wave at or before that year; earliest available HBS is used if no prior wave exists. | specific_hbs_year, interpolated_hbs, or custom_hbs/custom_index_weights. | Yes | This is one of the main places where country-specific metadata can improve defaults. |
| HBS interpolation | No interpolation by default. | interpolated_hbs = TRUE. | Yes | Useful when HBS waves are sparse and users want smoother weights over time. |
| Specific HBS wave | No single HBS wave is forced by default. | specific_hbs_year. | Yes | Useful for sensitivity analysis or reproducing a fixed-wave methodology. |
| Incomplete CPI coverage | Missing CPI series are not synthesised by default. | ensure_complete_cpi = TRUE. | Yes | Synthesised CPI data use parent-category price movements. |
| Inflation aggregation | Inflation is computed from COICOP contributions using annual HICP weights adjusted by HBS relative expenditure shares. | custom_cpi, custom_index_weights, custom_hbs, level, and date arguments. | Yes |  |
| Belgium (BE) | No Belgium-specific override yet; Eurostat harmonised HBS income categories are used if available. | custom_hbs; future country metadata could set quartile defaults. | Partly | Belgium is the strongest candidate for a country-specific quartile rule. |
| Italy (IT) | If the Eurostat 2010 income HBS wave is available, Italy uses that wave for all CPI weight years; other Eurostat HBS waves are not used by default. | custom_hbs can still override the default; specific_hbs_year can force a different Eurostat wave when requested. | Partly | The package default does not use national Italian HBS data because earlier I.Stat sources identify expenditure quintiles rather than income quintiles. |
