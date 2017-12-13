# fitur

The goal of fitur is to provide a wrapper function to take numeric data and fit the parameters to the specified distribution. The function returns an object that has the d, p, q, r family family functions with the fitted parameters.

## Installation

You can install fitur from CRAN or github with:

```R
install.packages('fitur')
devtools::install_github("tomroh/fitur")
```

## Example

This is a basic example to fit a poisson distribution with new parameters and return the functions for it.

```R
set.seed(562)
x <- rpois(100, 1)
fittedPois <- fit_univariate(x, 'pois', 'discrete')
fittedPois$dpois(1)
fittedPois$ppois(1)
fittedPois$qpois(.5)
fittedPois$rpois(100)
fittedPois$parameters
```
