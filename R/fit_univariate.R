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
#' an MLE fitted function family
#'
#' @import fitdistrplus
#'
#' @export
#'
#' @examples
fit_univariate <- function(x, family, type = 'continuous') {

  #'hyper', 'dunif', 'empirical'

  discreteFam <- c('geom', 'nbinom', 'pois')


  if (type %in% 'discrete') {

    stopifnot(is.integer(x))

    if (family %in% discreteFam) {
      return(build_dist(x, family))
    }
    else if (family %in% 'empirical') {

    }
    else {
      message("family not in supported discrete distributions")
    }
  } else if (type %in% 'continuous') {

  }



  if (family %in% c('empirical')) {
    # create master table
    # manual build of d (subset to name), p is cumulative,
    # q is inverse of 0-1 percentile (rounding), for r use sample
    #parameters <- fitdist(x, distr = family)[['estimate']]
  }

}



test <- fit_univariate(rpois(100000, 5), 'geom', 'discrete')

