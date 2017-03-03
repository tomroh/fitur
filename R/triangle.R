#' The Triangle Distribution
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
#' minimum value of distribution
#'
#' @param mode
#'
#' mode value of distribution
#'
#' @param max
#'
#' maximum value of distribution
#'
#' @return
#'
#' dtriangle gives the density, ptriangle gives the distribution function,
#' qtriangle gives the quantile function, rtriangle generates random deviates
#'
#' @export
#'
#' @examples
#' ddunif(0:1)
#' pdunif(1)
#' qdunif(.5)
#' rdunif(10)
#' @name Triangle

#' @rdname Triangle
#' @export
dtriangle <- function(x, min=0, mode = .5, max=1) {
  if (x >= min & x <= mode) {
    2*(x-min)/((max-min)*(mode-min))
  } else if (x > mode & x <= max) {
    2*(max-x)/((max-min)*(max-mode))
  } else {
    0
  }
}

#' @rdname Triangle
#' @export
ptriangle <- function(q, min=0, mode = .5, max=1) {
  if (q >= min & q <= mode) {
    (q-min)^2/((max-min)*(mode-min))
  } else if (q > mode & q <= max) {
    1-(max-q)^2/((max-min)*(max-mode))
  } else if (q < min) {
    0
  } else if (q > max) {
    1
  } else {
    NA
  }
}

#' @rdname Triangle
#' @export
qtriangle <- function(p, min=0, mode = .5, max=1) {
  floor(p*(max-min+1))
}

#' @rdname Triangle
#' @export
rtriangle <- function(n, min=0, mode = .5, max=1) {
  sample(min:max, n, replace=TRUE)
}
