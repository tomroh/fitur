#' Fit Empirical Distribution
#'
#' @param x
#'
#' integer or double vector
#'
#' @return
#'
#' if integer vector then list of family functions for d, p, q, r, and parameters
#' based on each integer value. if it is a double vector then
#' list of family functions for d, p, q, r, and parameters based on
#' Freedman-Diaconis rule for optimal number of histogram bins.
#'
#' @export
#'
#' @examples
#' set.seed(562)
#' x <- rpois(100, 5)
#' empDis <- fit_empirical(x)
#'
#' # probability density function
#' plot(empDis$dempDis(0:10),
#'      xlab = 'x',
#'      ylab = 'dempDis')
#' # cumulative distribution function
#' plot(x = 0:10,
#'      y = empDis$pempDis(0:10),
#'      #type = 'l',
#'      xlab = 'x',
#'      ylab = 'pempDis')
#' # quantile function
#' plot(x = seq(.1, 1, .1),
#'      y = empDis$qempDis(seq(.1, 1, .1)),
#'      type = 'p',
#'      xlab = 'x',
#'      ylab = 'qempDis')
#' # random sample from fitted distribution
#' summary(empDis$r(100))
#'
#' empDis$parameters
#'
#' set.seed(562)
#' x <- rexp(100, 1/5)
#' empCont <- fit_empirical(x)
#'
#' # probability density function
#' plot(x = 0:10,
#'      y = empCont$dempCont(0:10),
#'      xlab = 'x',
#'      ylab = 'dempCont')
#' # cumulative distribution function
#' plot(x = 0:10,
#'      y = empCont$pempCont(0:10),
#'      #type = 'l',
#'      xlab = 'x',
#'      ylab = 'pempCont')
#' # quantile function
#' plot(x = seq(.5, 1, .1),
#'      y = empCont$qempCont(seq(.5, 1, .1)),
#'      type = 'p',
#'      xlab = 'x',
#'      ylab = 'qempCont')
#' # random sample from fitted distribution
#' summary(empCont$r(100))
#'
#' empCont$parameters
fit_empirical <- function(x) {
  stopifnot(is.double(x) | is.integer(x))

  if (is.integer(x)) {
    fit_empirical_discrete(x)
  } else {
    fit_empirical_continuous(x)
  }
}

fit_empirical_discrete <- function(x) {
  stopifnot(is.integer(x))
  x <- sort(x)
  values <- unique(x)
  # convert to factors so tabulate doesn't ignore numbers <= 0
  probs <- tabulate(as.factor(x))/length(x)
  names(probs) <- values

  d <- Vectorize(function(x) {
    probs[x == values]
  })

  p <- Vectorize(function(q) {
    sum(probs[q >= values])
  })

  q <- Vectorize(function(p) {
    if (p < min(values) | p > max(values)) {
      warning("NaNs produced", call. = FALSE)
      NaN
    } else {
      max(values[cumsum(probs) <= p])
    }
  })

  r <- function(n) {
    sample(x = values, size = n, prob = probs, replace = TRUE)
  }
  list(dempDis = d,
       pempDis = p,
       qempDis = q,
       rempDis = r,
       parameters = probs)
}

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

  d <- Vectorize(function(x) {
    if ( x > max(rightEnds) | x < min(leftEnds)) {
      0
    } else {
      probs[x >= leftEnds & x < rightEnds]
    }
  })

  p <- Vectorize(function(q) {
    sum(probs[q >= leftEnds])
  })

  q <- Vectorize(function(p) {
    if (p < min(mids) | p > max(mids)) {
      warning("NaNs produced", call. = FALSE)
      NaN
    } else {
      max(mids[cumsum(probs) <= p])
    }
  })

  r <- function(n) {
    sample(x = mids, size = n, prob = probs, replace = TRUE)
  }
  list(dempCont = d,
       pempCont = p,
       qempCont = q,
       rempCont= r,
       parameters = probs)
}

get_interval_nums <- function(cuts) {
  numChars <- strsplit(gsub('\\(|\\]|\\)|\\]', "", cuts), ",")
  lapply(numChars, as.numeric)
}
