#' Fit Univariate Distribution
#'
#' @param x
#'
#' numeric vector
#'
#' @param family
#'
#' character name of distribution family
#'
#' @param type
#'
#' discrete or continuous data
#'
#' @return
#'
#' a fitted function family, MLE for probability distributions, custom fit for empirical (see functions)
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
fit_univariate <- function(x, family, type = 'continuous') {

  stopifnot(is.numeric(x))
  stopifnot(type %in% c('discrete', 'continuous'))

  # hyper, dunif, empirical
  discreteDists <- c('geom', 'nbinom', 'pois', 'dunif')
  continuousDists <- c('exp', 'cauchy', 'gamma', 'lnorm',
                     'norm', 'unif', 'weibull', 'llogis', 'logis',
                     'invweibull', 'invgamma')

  if (family %in% 'empirical') {
    fit_empirical(x)
  } else if (type %in% 'discrete') {
    stopifnot(is.integer(x))
    if (family %in% discreteDists) {
      return(build_dist(x, family))
    }
    else {
      message("family not in supported discrete distributions")
    }
  } else if (type %in% 'continuous') {
    stopifnot(is.double(x))
    if (family %in% continuousDists) {
      return(build_dist(x, family))
    }
    else {
      message("family not in supported continuous distributions")
    }
  }
}
