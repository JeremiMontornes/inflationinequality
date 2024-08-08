---
title: "README"
author: Pradana AUMARS, Jérémi MONTORNÈS
date: 2024-07-31
---

# inflationinequality

`inflationinequality` provides methods to calculate and visualize inflation inequality indicators.

## Features

* Calculate and visualize inflation and contributions to inflation by households categories
* Simulate counterfactual price indices

## To-do

#### Bug-fixing

- [ ] Reach 100% code coverage
- [x] Connection error to Dbnomics  (add option readlines)
- [ ] Fix issue merge level 2 - level 3 Insee
- [x] check : simulation_cpi() : for coicop ="011" large shocks (+40%) =>total > Q1 or Q5 but not true for every coicop


#### Simulation

- [x] Implement simple simulation algorithm

#### Documentation

- [ ] correct ?calculate_inflation() level 02  "For example, "01" is level 1 and "012" is level 2."
- [ ] Improve writing and explanation in vignettes (for instance How to implement level=3 for France)
- [ ] Improve writing and explanation in vignettes : interpolation time varying weithts
- [ ] Improve writing and explanation in vignettes : reduce the number of pages / suppress some outputs of the vignette "Verifying calculated inflation" delete compute_diagnostics() computationnaly too intensive
- [ ] Re-adjust wording of certain terms: offset vs. gap?
- [ ] Re-adjust wording of certain terms: avoid "our package" prefer "the package" (already corrected where I saw)
- [ ] Sections of the working paper that are related to the package word document to save in the folder \\adbdf.private\applications\AU_AMIC\LPR_2022_SAMIC\12-DT

#### Test

- [ ] 19 August  Beta test of inflationinequality package

#### Maintenance

- [ ] 19 August/23 August Migrate to Jérémi's GitHub account in a private repository

## License

MIT
