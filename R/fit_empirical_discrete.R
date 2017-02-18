#' Fit Empirical Discrete Distribution Family
#'
#' @param x
#'
#' integer vector
#'
#' @return
#'
#' family of probability distribution functions
#'
#' @export
#'
#' @examples
#' set.seed(562)
#' x <- rpois(100, 5)
#' empDis <- fit_empirical_discrete(x)
#' empDis$d(1)
#' empDis$p(2)
#' empDis$q(1)
#' table(empDis$r(10000))/10000

fit_empirical_discrete <- function(x) {
  stopifnot(is.integer(x))
  x <- sort(x)
  values <- unique(x)
  # convert to factors so tabulate doesn't ignore numbers <= 0
  probs <- tabulate(as.factor(x))/length(x)

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
  list(d = d, p = p, q = q, r= r)

}
