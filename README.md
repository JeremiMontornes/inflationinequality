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
- [ ] fix bug : simulation_cpi() : large shocks (+40%) =>total > Q1 or Q5
- [ ] fix bug : calculate_inflation() coicop level 3 comparison with results in the REF article

#### Simulation

- [x] Implement simple simulation algorithm

#### Documentation

- [ ] Finish sections of the working paper that are related to the package
- [ ] Re-adjust wording of certain terms 
- [ ] Improve writing and explanation in vignettes 

#### Maintenance

- [ ] Migrate to Jérémi's GitHub account in a private repository

## License

MIT
