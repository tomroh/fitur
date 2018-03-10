# library(fitdistrplus)
# set.seed(562)
# x <- rpois(100, 1)
# fittedPois <- fit_univariate(x, 'pois', 'discrete')
# fittedPois$d(1)
# fittedPois$p(1)
# fittedPois$q(.5)
# fittedPois$r(100)
# fittedPois$parameters
#
# # Compare to explicitly
# lambda <- fitdist(x, distr = 'pois')[['estimate']]
# dpois(1, lambda)
# ppois(1, lambda)
# qpois(.5, lambda)
# rpois(100, lambda)
#
# set.seed(562)
# x <- rexp(100, 1)
# fittedExp <- fit_univariate(x, 'exp')
# fittedExp$d(1)
# fittedExp$p(1)
# fittedExp$q(.5)
# fittedExp$r(100)
#
# # Compare to explicitly
# rate <- fitdist(x, distr = 'exp')[['estimate']]
# dexp(1, rate)
# pexp(1, rate)
# qexp(.5, rate)
# rexp(100, rate)
# cumsum(ddist), pdist
# qdist(pdist(x) == x

# probs <- seq(0, 1, .01)
# pnorm(probs)
# all.equal(probs, qnorm(pnorm(probs)))
# sum(dnorm(probs))
# pdf <- density(rnorm(100000000))
# all.equal(dnorm(pdf$x), pdf$y)
# assertthat::are_equal(dnorm(pdf$x), pdf$y)
# View(data.frame(dnorm(pdf$x), pdf$y))
