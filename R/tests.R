# library(fitur)
# library(actuar)
#
# # 'geom' test is currently not working
# discreteDists <- c('nbinom', 'pois', 'dunif')
# # 'unif' is hypersensitive to fitting at the bounds
# continuousDists <- c('exp', 'cauchy', 'gamma', 'lnorm',
#                      'norm', 'weibull', 'llogis', 'logis',
#                      'invweibull', 'invgamma')
# set.seed(82)
# for( i in seq_along(continuousDists) ) {
#
#   x <- runif(1000)
#   probs <- seq(0, 1, .01)
#   fit <- fit_univariate(x, continuousDists[i])
#   testProbs <- fit[[3]](fit[[2]](probs))
#   if(!isTRUE(all.equal(probs, testProbs, tolerance = 1.5e-7))) {
#     stop(paste0(continuousDists[i], " failed p-q test"))
#   }
#
# }

# for( i in seq_along(discreteDists) ) {
#
#   x <- rpois(1000, 1)
#   probs <- seq(0, 1, .01)
#   fit <- fit_univariate(x, discreteDists[i], type = 'discrete')
#   testProbs <- fit[[3]](fit[[2]](probs))
#   if(!isTRUE(all.equal(probs, testProbs, tolerance = 1.5e-7))) {
#     stop(paste0(discreteDists[i], " failed p-q test"))
#   }
#
# }

