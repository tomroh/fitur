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
#' empDis <- fit_empirical_uniform(x)
#' empDis$d(1)
#' empDis$p(2)
#' empDis$q(1)
#' table(empDis$r(10000))/10000

fit_empirical_discrete <- function(x) {
  stopifnot(is.integer(x))
  x <- sort(x)
  values <- unique(x)
  probs <- tabulate(x)/length(x)

  d <- function(x) {
    probs[x == values]
  }

  p <- function(q) {
    sum(probs[q <= values])
  }

  q <- function(p) {
    max(values[cumsum(probs) <= p])
  }

  r <- function(n) {
    sample(x = values, size = n, prob = probs, replace = TRUE)
  }
  list(d = d, p = p, q = q, r= r)

}

# testfit <- fit_empirical_uniform(as.integer(x))
# testfit$d(1)
# testfit$p(2)
# testfit$q(1)
# table(testfit$r(10000))/10000

