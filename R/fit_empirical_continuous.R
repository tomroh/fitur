library(stringr)
x <- rpois(100, 5)
fit_empirical_continuous <- function(x) {
  stopifnot(is.double(x))
  x <- sort(x)
  binwidth <- diff(range(x)) / (2 * IQR(x) / length(x)^(1/3))
  bins <- sapply(split(x, cut(x, binwidth)), length)
  values <- calc_mid(names(bins))
  probs <- bins/sum(bins)

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

calc_mid <- function(cuts) {
  nums <- strsplit(gsub('\\(|\\]|\\)|\\]', "", cuts), ",")
  sapply(nums, function(x) mean(as.numeric(x)))
}

