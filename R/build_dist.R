#' Build Distribution Functions
#'
#' A wrapper for building function families given
#' a numeric vector and the distribution family
#'
#' @param x
#'
#' numeric vector
#'
#' @param family
#'
#' distribution family character name
#'
#' @return list of family functions for d, p, q, r
#'
#' @import fitdistrplus actuar
#'
#' @export
#'
#' @examples
#' fittedDists <- build_dist(rpois(100,5), 'pois')
#' dpois(x = 5, lambda = 5)
#' fittedDists$d(5)
#' ppois(5, 5)
#' fittedDists$p(5)
#' qpois(.5, 5)
#' fittedDists$q(.5)
#' set.seed(8257)
#' rpois(100, 5)
#' set.seed(8257)
#' fittedDists$r(100)


build_dist <- function(x, family) {
  # generate list of distribution functions
  type <- c('d', 'p', 'q', 'r')
  funs <- lapply(type, function(type) {
    get(paste0(type, family))
  })
  names(funs) <- type
  params <- fitdist(data = x, distr = family)[['estimate']]
  lapply(setNames(funs, names(funs)), gen_dist_fun,
         parameters = params)
}

#' Generate Single Distribution Function
#'
#' @param f
#'
#' one of distribution functions
#'
#' @param parameters
#'
#' new parameters for distribution
#'
#' @param ...
#'
#' arguments to pass on to distribution function
#'
#' @return
#' one of parameterized distribution functions in d, p, q, r

gen_dist_fun <- function(f, parameters, ...) {
  function(...)
    do.call(f, c(list(...), parameters))
}
