#' Provide Summary Stats for Inspecting Numeric Data
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

#' Fit Univariate Distributions by Specifying Parameters
#'
#' @param family
#'
#' distribution family character name
#'
#' @param parameters
#'
#' named vector of parameters to set
#'
#' @return
#'
#' list of family functions for d, p, q, r, and parameters
#'
#' @export
#'
#' @examples
#' manFun <- fit_univariate_man('norm', c(mean = 2, sd = 5))
#' set.seed(5)
#' m1 <- mean(manFun$rnorm(100000))
#' set.seed(5)
#' m2 <- mean(rnorm(100000, 2, 5))
#' identical(m1, m2)
fit_univariate_man <- function(family, parameters) {

  type <- paste0(c('d', 'p', 'q', 'r'), family)
  funs <- lapply(type, function(type) {
    match.fun(type)
  })
  names(funs) <- type

  # parameter name checking
  allParams <- unique(names(unlist(lapply(unname(funs), formals))))
  specParams <- names(parameters)
  matchedArgs <- match.arg(specParams, allParams, several.ok = TRUE)
  if (length(matchedArgs) != length(specParams)) {
    stop("Specified names of parameters do not match argument names of functions")
  }

  funs <- lapply(setNames(funs, names(funs)),
                 gen_dist_fun,
                 parameters = parameters)
  funs[['parameters']] <- parameters
  funs

}

#' Find Mode
#'
#' @param x
#'
#' vector of data
#'
#' @return
#'
#' mode of data
#'
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
