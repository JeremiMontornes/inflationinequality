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
- [ ] Add more error checking and remove the silly ones
- [ ] check : simulation_cpi() : for coicop ="011" large shocks (+40%) =>total > Q1 or Q5 but not true for every coicop
- [ ] check : calculate_inflation() coicop level 3 comparison with results in the REF article

#### Simulation

- [x] Implement simple simulation algorithm

#### Documentation

- [ ] Finish sections of the working paper that are related to the package
- [ ] Re-adjust wording of certain terms 
- [ ] correct ?calculate_inflation() level 02  "For example, "01" is level 1 and "012" is level 2."
- [ ] Improve writing and explanation in vignettes (for instance How to implement level=3 for France)

#### Maintenance

- [ ] Migrate to Jérémi's GitHub account in a private repository

## License

MIT
