# fitur

The goal of fitur is to provide a wrapper function to take numeric data and fit the parameters to the specified distribution. The function returns and object that has the d, p, q, r family family functions with the fitted parameters.

## Installation

You can install fitur from github with:

```R
# install.packages("devtools")
devtools::install_github("fitur/datareaver")
```

## Example

This is a basic example to fit a poisson distribution with new parameters and return the functions for it.

```R
set.seed(562)
x <- rpois(100, 1)
fittedPois <- fit_univariate(x, 'pois', 'discrete')
fittedPois$d(1)
fittedPois$p(1)
fittedPois$q(.5)
fittedPois$r(100)
...
```
