#' Fit Continous Empirical Distribution Family
#'
#' @param x
#'
#' numeric vector
#'
#' @return
#'
#' list of family functions for d, p, q, r, and parameters
#'
#' @import stats
#'
#' @export
#'
#' @examples
#' set.seed(562)
#` x <- rexp(100, 5)
#` empCont <- fit_empirical_continuous(x)
#` empCont$dempCont(.2)
#` empCont$pempCont(.2)
#` empCont$qempCont(.8)
#` empCont$rempCont(100)
#` empCont$parameters
fit_empirical_continuous <- function(x) {
  stopifnot(is.double(x))
  x <- sort(x)
  # Freedman-Diaconis rule for optimal number of histogram bins
  nbins <- diff(range(x)) / (2 * IQR(x) / length(x)^(1/3))
  bins <- sapply(split(x, cut(x, nbins)), length)
  intervals <- get_interval_nums(names(bins))
  # use midpoint of intervals for return values
  # small rounding errors
  mids <- sapply(intervals, mean)
  leftEnds <- sapply(intervals, min)
  rightEnds <- sapply(intervals, max)
  probs <- bins/sum(bins)

  d <- function(x) {
    if ( x > max(rightEnds) | x < min(leftEnds)) {
      0
    } else {
      probs[x >= leftEnds & x < rightEnds]
    }
  }

  p <- function(q) {
    sum(probs[q >= leftEnds])
  }

  q <- function(p) {
    max(mids[cumsum(probs) <= p])
  }

  r <- function(n) {
    sample(x = mids, size = n, prob = probs, replace = TRUE)
  }
  list(dempCont = d, pempCont = p, qempCont = q, rempCont= r,
       parameters = probs)
}

get_interval_nums <- function(cuts) {
  numChars <- strsplit(gsub('\\(|\\]|\\)|\\]', "", cuts), ",")
  lapply(numChars, as.numeric)
}


