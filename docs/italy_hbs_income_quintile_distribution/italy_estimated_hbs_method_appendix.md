# Appendix: estimated Italy HBS income-quintile baskets for 2015 and 2020

Eurostat reports Italian HBS consumption baskets by income quintile for 2005,
but the harmonised income-quintile table does not provide complete Italian
income-quintile baskets for the 2015 and 2020 HBS waves. To keep Italy in
income-quintile calculations while avoiding expenditure-quintile data, we
construct estimated 2015 and 2020 income-quintile HBS baskets from the observed
2005 income-quintile structure and the all-households Italian HBS basket in the
target year.

Let \(E_{j,g,2005}\) denote observed HBS expenditure on COICOP product category
\(j\) for income quintile \(g\) in 2005. Since income quintiles are equal
population groups, the 2005 all-households reference for product \(j\) is
approximated by the simple mean across the five quintiles:

\[
\bar E_{j,2005}
= \frac{1}{5}\sum_{g=1}^{5} E_{j,g,2005}.
\]

The observed 2005 relative expenditure intensity of income quintile \(g\) for
product \(j\) is then:

\[
c_{j,g,2005}
= \frac{E_{j,g,2005}}{\bar E_{j,2005}}.
\]

For a target HBS wave \(y \in \{2015, 2020\}\), let \(E_{j,all,y}\) denote the
all-households Italian HBS expenditure for product \(j\). The estimated
income-quintile expenditure is:

\[
\widehat E_{j,g,y}
= c_{j,g,2005} \times E_{j,all,y}.
\]

The estimated product shares used as HBS inputs are finally normalised within
each income quintile and target year:

\[
\widehat s_{j,g,y}
= \frac{\widehat E_{j,g,y}}
{\sum_{k \in \mathcal{J}_y} \widehat E_{k,g,y}}.
\]

In words, the method keeps the cross-quintile relative expenditure pattern
observed in the 2005 Italian income-quintile HBS and updates the aggregate
Italian consumption basket to the 2015 or 2020 HBS wave. It therefore captures
changes in the national all-households consumption structure, but it does not
capture any change between income quintiles after 2005. A fuller approach would
impute household income into the Italian HBS microdata using EU-SILC as donor
data and statistical matching, then rebuild observed income quintiles directly
inside the HBS.

The resulting object is saved as:

`data-raw/italy_estimated_hbs/IT_income_hbs_estimated_2015_2020_from_2005_level2.rds`

It is an `inflationinequality::hbs()` object and can be passed to
`calculate_weights()` through `custom_hbs`.
