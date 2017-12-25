#' Build Distribution Functions
#'
#' A wrapper for building function families given
#' a numeric vector and the distribution
#'
#' @param x
#'
#' numeric vector
#'
#' @param distribution
#'
#' distribution character name
#'
#' @return list of distribution functions for d, p, q, r, and parameters
#'
#' @import fitdistrplus actuar
#'
#' @export
#'
#' @examples
#' fittedDists <- build_dist(rpois(100,5), 'pois')
#' dpois(x = 5, lambda = 5)
#' fittedDists$dpois(5)
#' ppois(5, 5)
#' fittedDists$ppois(5)
#' qpois(.5, 5)
#' fittedDists$qpois(.5)
#' set.seed(8257)
#' rpois(100, 5)
#' set.seed(8257)
#' fittedDists$rpois(100)
#' fittedDists$parameters


build_dist <- function(x, distribution) {
  # generate list of distribution functions
  type <- paste0(c('d', 'p', 'q', 'r'), distribution)
  funs <- lapply(type, function(type) {
    match.fun(type)
  })
  names(funs) <- type

  if (distribution %in% 'dunif') {
    parameters <- c(min = min(x), max = max(x))
  } else {
    parameters <- fitdist(data = x, distr = distribution)[['estimate']]
  }
  funs <- lapply(setNames(funs, names(funs)), gen_dist_fun,
         parameters = parameters)
  funs[['parameters']] <- parameters
  funs
}
