#' Fit Empirical Discrete Distribution Family
#'
#' @param x
#'
#' integer vector
#'
#' @return
#'
#' list of family functions for d, p, q, r, and parameters
#'
#' @export
#'
#' @examples
#' set.seed(562)
#' x <- rpois(100, 5)
#' empDis <- fit_empirical_discrete(x)
#' empDis$dempDis(1)
#' empDis$pempDis(2)
#' empDis$qempDis(1)
#' empDis$r(10000)
#' empDis$parameters

fit_empirical_discrete <- function(x) {
  stopifnot(is.integer(x))
  x <- sort(x)
  values <- unique(x)
  # convert to factors so tabulate doesn't ignore numbers <= 0
  probs <- tabulate(as.factor(x))/length(x)
  names(probs) <- values

  d <- function(x) {
    probs[x == values]
  }

  p <- function(q) {
    sum(probs[q >= values])
  }

  q <- function(p) {
    max(values[cumsum(probs) <= p])
  }

  r <- function(n) {
    sample(x = values, size = n, prob = probs, replace = TRUE)
  }
  list(dempDis = d, pempDis = p, qempDis = q, rempDis = r,
       parameters = probs)
}
