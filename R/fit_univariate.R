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
#' fittedPois$d(1)
#' fittedPois$p(1)
#' fittedPois$q(.5)
#' fittedPois$r(100)
#'
#' # Fit Continuous Distribution
#' fittedExp <- fit_univariate(rexp(100, 1), 'exp')
#' fittedExp$d(1)
#' fittedExp$p(1)
#' fittedExp$q(.5)
#' fittedExp$r(100)


fit_univariate <- function(x, family, type = 'continuous') {

  stopifnot(is.numeric(x))
  stopifnot(type %in% c('discrete', 'continuous'))

  # hyper, dunif, empirical
  discreteFam <- c('geom', 'nbinom', 'pois')
  continuousFam <- c('exp', 'gamma', 'lnorm', 'norm', 'unif', 'weibull')

  if (family %in% 'empirical') {
    if(type %in% 'discrete') {
      fit_empirical_discrete(x)
    }
    else {
      fit_empirical_continuous(x)
    }
  }

  else if (type %in% 'discrete') {
    stopifnot(is.integer(x))
    if (family %in% discreteFam) {
      return(build_dist(x, family))
    }
    else {
      message("family not in supported discrete distributions")
    }
  } else if (type %in% 'continuous') {
    stopifnot(is.numeric(x))
    if (family %in% continuousFam) {
      return(build_dist(x, family))
    }
    else {
      message("family not in supported continuous distributions")
    }
  }
}


#TODO explore using bound argument
#TODO offset argument, automate with small number?
