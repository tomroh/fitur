#' Fit Univariate Distribution
#'
#' @param x
#'
#' numeric vector
#'
#' @param distribution
#'
#' character name of distribution
#'
#' @param type
#'
#' discrete or continuous data
#'
#' @return
#'
#' a fitted list object of d, p, q, r distribution functions and parameters,
#' MLE for probability distributions, custom fit for empirical
#'
#' @export
#'
#' @examples
#' # Fit Discrete Distribution
#' set.seed(42)
#' x <- rpois(1000, 3)
#' fitted <- fit_univariate(x, 'pois', type = 'discrete')
#' # density function
#' plot(fitted$dpois(x=0:10),
#'      xlab = 'x',
#'      ylab = 'dpois')
#' # distribution function
#' plot(fitted$ppois(seq(0, 10, 1)),
#'      xlab= 'x',
#'      ylab = 'ppois')
#' # quantile function
#' plot(fitted$qpois,
#'      xlab= 'x',
#'      ylab = 'qpois')
#' # sample from theoretical distribution
#' summary(fitted$rpois(100))
#' # estimated parameters from MLE
#' fitted$parameters
#'
#' set.seed(24)
#' x <- rweibull(1000, shape = .5, scale = 2)
#' fitted <- fit_univariate(x, 'weibull')
#' # density function
#' plot(fitted$dweibull,
#'      xlab = 'x',
#'      ylab = 'dweibull')
#' # distribution function
#' plot(fitted$pweibull,
#'      xlab = 'x',
#'      ylab = 'pweibull')
#' # quantile function
#' plot(fitted$qweibull,
#'      xlab = 'x',
#'      ylab = 'qweibull')
#' # sample from theoretical distribution
#' summary(fitted$rweibull(100))
#' # estimated parameters from MLE
#' fitted$parameters
fit_univariate <- function(x, distribution, type = 'continuous') {

  stopifnot(type == 'discrete' & is.integer(x) |
            type == 'continuous' & is.double(x) |
            type == 'empirical' & is.numeric(x))
  discreteDists <- c('geom', 'nbinom', 'pois', 'dunif')
  continuousDists <- c('exp', 'cauchy', 'gamma', 'lnorm',
                     'norm', 'unif', 'weibull', 'llogis', 'logis',
                     'invweibull', 'invgamma')
  if (!distribution %in% c(discreteDists, continuousDists, 'empirical')) {
    stop("distribution not in supported distributions")
  }
  if (distribution %in% 'empirical') {
    fit_empirical(x)
  } else {
    build_dist(x, distribution)
  }

}
# use structure to turn into class "distfun"
# think about creating hierarchiny class "distsfun"
