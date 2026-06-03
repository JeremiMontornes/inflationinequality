# Appendix: latent-income probability method for Italy HBS 2015 and 2020

The Italian HBS public-use files for 2015 and 2020 do not provide household
income in a form that directly identifies income quintiles. Eurostat reports an
observed Italian HBS consumption basket by income quintile for 2005, but the
corresponding 2005 Istat microdata are not used here. The method therefore
calibrates at the aggregate basket level, not at the individual level.

For each household \(i\) in the 2015 or 2020 Italian HBS microdata, let \(X_i\)
denote observed socio-economic characteristics: household size, age and
education of the reference person, employment and occupational variables,
housing tenure, surface and rooms per household member, ownership of durable
goods, internet access, region, macro-region, and subjective economic-situation
variables.

We define a latent socio-economic score:

\[
z_i = X_i \beta .
\]

Households are then assigned probabilistically to five latent income groups. Let
\(r_i(\beta)\) be the weighted rank of \(z_i\) in the HBS sample. The probability
that household \(i\) belongs to group \(g \in \{1,\dots,5\}\) is:

\[
p_{i,g}(\beta)
=
\frac{
\exp\{-\tau [r_i(\beta)-m_g]^2\}
}{
\sum_{h=1}^{5}\exp\{-\tau [r_i(\beta)-m_h]^2\}
},
\]

where \(m_g\) is the centre of quintile \(g\), namely
\((0.1, 0.3, 0.5, 0.7, 0.9)\), and \(\tau\) controls how sharply households are
assigned around each quintile centre.

For a COICOP division \(j\), the probabilistic basket share of latent group
\(g\) in target year \(y \in \{2015, 2020\}\) is:

\[
\widehat s_{j,g,y}(\beta)
=
\frac{
\sum_i w_{i,y} p_{i,g,y}(\beta) e_{i,j,y}
}{
\sum_k \sum_i w_{i,y} p_{i,g,y}(\beta) e_{i,k,y}
},
\]

where \(w_{i,y}\) is the HBS household weight and \(e_{i,j,y}\) is household
expenditure on COICOP division \(j\).

The parameter vector \(\beta\) is chosen so that the target-year probabilistic
baskets reproduce as closely as possible the observed 2005 Eurostat gradient by
income quintile:

\[
\widehat\beta_y
=
\arg\min_\beta
\sum_{g=1}^{5}\sum_j
\left[
\widehat s_{j,g,y}(\beta)
-
s^{Eurostat}_{j,g,2005}
\right]^2
+ \lambda \|\beta\|^2 .
\]

This is an aggregate calibration. The 2005 data provide the target distribution
of consumption by income quintile and product division; the 2015 and 2020
microdata provide the household characteristics and observed expenditures used
to build probabilistic latent-income groups.

After calibration, division-level latent-income intensities are applied to the
all-households Italian HBS totals in the target year. This gives an
`inflationinequality::hbs()` object compatible with `calculate_weights()`.

The method improves on a purely aggregate projection because it uses the
composition of households and their observed expenditure patterns in 2015 and
2020. Its main limitation is that the groups remain latent income groups:
without EU-SILC or another donor file containing household income, the method
cannot identify actual observed income quintiles at the household level.
