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
fit_univariate <- function(x, family, type) {

  #'hyper'

  discreteFam <- c('geom', 'empirical',
                   'nbinom', 'pois', 'dunif')


  if (type %in% 'discrete') {

    stopifnot(is.integer(x))

    if (family %in% c('geom', 'pois', 'nbinom', 'unif')) {
      parameters <- fitdist(x, distr = family)[['estimate']]
      build_dist(x, family)
    }
    else if (family %in% 'empirical') {
    }
    else {
      message("family not in supported discrete distributions")
    }
  }



  if (family %in% c('empirical')) {
    # create master table
    # manual build of d (subset to name), p is cumulative,
    # q is inverse of 0-1 percentile (rounding), for r use sample
    parameters <- fitdist(x, distr = family)[['estimate']]
  }
  FUNS <- c('d', 'p', 'q', 'r')
  lapply(setNames(FUNS, FUNS), function(f) {
    newFUN <- get(paste0(f, family))
    for(parameter in names(parameters)) {
      formals(newFUN)[[parameter]] <- parameters[[parameter]]
    }
    newFUN
  })
  list(d = ,
       p = ,
       q = ,
       r = )

}




