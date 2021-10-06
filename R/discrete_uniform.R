#' The Discrete Uniform Distribution
#'
#' @param x
#'
#' vector of (non-negative integer) quantiles
#'
#' @param q
#'
#' vector of quantiles
#'
#' @param p
#'
#' vector of probabilities
#'
#' @param n
#'
#' number of random values to return
#'
#' @param min
#'
#' minimum value of distribution (integer)
#'
#' @param max
#'
#' maximum value of distribution (integer)
#'
#' @return
#'
#' ddunif gives the density, pdunif gives the distribution function,
#' qdunif gives the quantile function, rdunif generates random deviates
#'
#' @export
#'
#' @examples
#' ddunif(0:1)
#' pdunif(1)
#' qdunif(.5)
#' rdunif(10)
#' @name DiscreteUniform

#' @rdname DiscreteUniform
#' @export
ddunif <- function(x, min=0, max=1) {
  ifelse(x>=min & x<=max & round(x)==x, 1/(max-min+1), 0)
}

#' @rdname DiscreteUniform
#' @export
pdunif <- function(q, min=0, max=1) {
  ifelse(q<min, 0, ifelse(q>=max, 1, (floor(q)-min+1)/(max-min+1)))
}

#' @rdname DiscreteUniform
#' @export
qdunif <- function(p, min=0, max=1) {
  floor(p*(max-min+1))
}

#' @rdname DiscreteUniform
#' @export
rdunif <- function(n, min=0L, max=1) {
  sample(seq(min, max, 1L), n, replace=TRUE)
}
