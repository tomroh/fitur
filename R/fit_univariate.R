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
#' fittedPois <- fit_univariate(rpois(100, 1), 'pois', 'discrete')
#' fittedPois$dpois(1)
#' fittedPois$ppois(1)
#' fittedPois$qpois(.5)
#' fittedPois$rpois(100)
#' fittedPois$parameters
#'
#' # Fit Continuous Distribution
#' fittedExp <- fit_univariate(rexp(100, 1), 'exp')
#' fittedExp$dexp(1)
#' fittedExp$pexp(1)
#' fittedExp$qexp(.5)
#' fittedExp$rexp(100)
#' fittedExp$parameters


fit_univariate <- function(x, family, type = 'continuous') {

  stopifnot(is.numeric(x))
  stopifnot(type %in% c('discrete', 'continuous'))

  # hyper, dunif, empirical
  discreteFam <- c('geom', 'nbinom', 'pois', 'dunif')
  continuousFam <- c('exp', 'cauchy', 'gamma', 'lnorm',
                     'norm', 'unif', 'weibull', 'llogis', 'logis',
                     'invweibull', 'invgamma')

  if (family %in% 'empirical') {
    fit_empirical(x)
  } else if (type %in% 'discrete') {
    stopifnot(is.integer(x))
    if (family %in% discreteFam) {
      return(build_dist(x, family))
    }
    else {
      message("family not in supported discrete distributions")
    }
  } else if (type %in% 'continuous') {
    stopifnot(is.double(x))
    if (family %in% continuousFam) {
      return(build_dist(x, family))
    }
    else {
      message("family not in supported continuous distributions")
    }
  }
}
