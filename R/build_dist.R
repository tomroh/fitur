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
  structure(funs,
            "distfun")
}

#' Fit Univariate Distributions by Specifying Parameters
#'
#' @param distribution
#'
#' distribution character name
#'
#' @param parameters
#'
#' named vector of parameters to set
#'
#' @return
#'
#' list of distribution functions for d, p, q, r, and parameters
#'
#' @import stats
#'
#' @export
#'
#' @examples
#' manFun <- fit_univariate_man('norm', c(mean = 2, sd = 5))
#' set.seed(5)
#' m1 <- mean(manFun$rnorm(100000))
#' set.seed(5)
#' m2 <- mean(rnorm(100000, 2, 5))
#' identical(m1, m2)
fit_univariate_man <- function(distribution, parameters) {

  type <- paste0(c('d', 'p', 'q', 'r'), distribution)
  funs <- lapply(type, function(type) {
    match.fun(type)
  })
  names(funs) <- type

  # parameter name checking
  allParams <- unique(names(unlist(lapply(unname(funs), formals))))
  specParams <- names(parameters)
  matchedArgs <- match.arg(specParams, allParams, several.ok = TRUE)
  if (length(matchedArgs) != length(specParams)) {
    stop("Specified names of parameters do not match argument names of functions")
  }

  funs <- lapply(setNames(funs, names(funs)),
                 gen_dist_fun,
                 parameters = parameters)
  funs[['parameters']] <- parameters
  structure(funs,
            "distfun")

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
#'
#' @return
#' one of parameterized distribution functions in d, p, q, r

gen_dist_fun <- function(f, parameters, ...) {
  function(...)
    do.call(f, c(list(...), parameters))
}
