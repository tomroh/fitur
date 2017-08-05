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

# fit_univariate_man <- function(distribution, parameters) {
#   params <- match.arg(c("mean", "sd"),
#                       names(formals(rnorm)),
#                       several.ok = TRUE)
#   gen
#
#
# }

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
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
#' @return
#' one of parameterized distribution functions in d, p, q, r

gen_dist_fun <- function(f, parameters, ...) {
  function(...)
    do.call(f, c(list(...), parameters))
}
