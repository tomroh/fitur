#' Utilities for fitur
#'
#' @param x
#'
#' a numeric vector
#'
#' @return
#'
#' a named vector of descriptive statistics
#'
#' @importFrom e1071 skewness kurtosis
#'
#' @export
#'
#' @examples
#' x <- rexp(1000, 2)
#' summarize_stats(x)
summarize_stats <- function(x) {
  stopifnot(is.numeric(x))
  c(quantile(x, probs = seq(0, 1, .01)),
    mean = mean(x),
    sd = sd(x),
    mode = Mode(x),
    skewness = skewness(x),
    kurtosis = kurtosis(x))
}

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
