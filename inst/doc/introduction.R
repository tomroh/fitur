## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(fitur)

## ---- discrete-----------------------------------------------------------
set.seed(562)
x <- rpois(100, 1)
fittedPois <- fit_univariate(x, 'pois', 'discrete')
fittedPois$d(1)
fittedPois$p(1)
fittedPois$q(.5)
fittedPois$r(100)

## ---- continuous---------------------------------------------------------
set.seed(562)
x <- rexp(100, 1)
fittedExp <- fit_univariate(x, 'exp')
fittedExp$d(1)
fittedExp$p(1)
fittedExp$q(.5)
fittedExp$r(100)

## ---- empirical----------------------------------------------------------
set.seed(562)
x <- rpois(100, 1)
fittedDEmp <- fit_univariate(x, family = 'empirical', type = 'discrete')
fittedDEmp$d(1)
fittedDEmp$p(1)
fittedDEmp$q(.5)
fittedDEmp$r(100)

set.seed(562)
x <- rexp(100, 1)
fittedCEmp <- fit_univariate(x, family = 'empirical', type = 'continuous')
fittedCEmp$d(1)
fittedCEmp$p(1)
fittedCEmp$q(.5)
fittedCEmp$r(100)

