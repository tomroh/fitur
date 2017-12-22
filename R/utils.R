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
#' @param distribution
#'
#' distribution character name
#'
#' @param parameters
#'
#' named vector of parameters to set
#'
#' @return
#'
#' list of distribution functions for d, p, q, r, and parameters
#'
#' @import stats
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
fit_univariate_man <- function(distribution, parameters) {

  type <- paste0(c('d', 'p', 'q', 'r'), distribution)
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

#' Q-Q Plot
#'
#' @param x
#'
#' numeric vector of sample data
#'
#' @param fits
#'
#' a list object produced from fit_univariate, fit_empirical, or
#' fit_univariate_man
#'
#' @return
#'
#' ggplot of quantile-quantile comparison of theoretical distribution
#'
#' @import ggplot2
#'
#' @export
#'
#' @examples
#' set.seed(37)
#' x <- rgamma(10000, 5)
#' dists <- c('gamma', 'lnorm', 'weibull')
#' fits <- lapply(dists, fit_univariate, x = x)
#' plot_qq(x, fits)
plot_qq <- function(x, fits) {
  theorQuant <- lapply(fits, function(fit) {
    # if(pmatch(c('d', 'p', 'q', 'r', 'parameters'), testNames))
    probs <- 1:length(x)/(length(x) + 1)
    data.frame(distribution = names(fit)[3],
               theoretical = sort(fit[[3]](probs)),
               stringsAsFactors = FALSE)
  })
  theorQuant <- do.call('rbind.data.frame', theorQuant)
  qq <- data.frame(sample = rep(sort(x), length(fits)),
                   theorQuant)
  ggplot(qq) +
    geom_point(aes(x = theoretical,
                   y = sample,
                   color = distribution)) +
    geom_abline(slope = 1,
                color = 'black')
}

#' P-P Plot
#'
#' @param x
#'
#' numeric vector of sample data
#'
#' @param fits
#'
#' a list object produced from fit_univariate, fit_empirical, or
#' fit_univariate_man
#'
#' @return
#'
#' ggplot of percentile-percentile comparison of theoretical distribution
#'
#' @import ggplot2
#'
#' @export
#'
#' @examples
#' set.seed(37)
#' x <- rgamma(10000, 5)
#' dists <- c('gamma', 'lnorm', 'weibull')
#' fits <- lapply(dists, fit_univariate, x = x)
#' plot_pp(x, fits)
plot_pp <- function(x, fits) {
  probs <- 1:length(x)/(length(x) + 1)
  theorPerc <- lapply(fits, function(fit) {
    data.frame(distribution = names(fit)[2],
               theoretical = sort(fit[[2]](sort(x))),
               stringsAsFactors = FALSE)
  })
  theorPerc <- do.call('rbind.data.frame', theorPerc)
  pp <- data.frame(sample = rep(probs, length(fits)),
                   theorPerc)
  ggplot(pp) +
    geom_point(aes(x = theoretical,
                   y = sample,
                   color = distribution)) +
    geom_abline(slope = 1,
                color = 'black') +
    theme_bw() +
    theme(panel.grid = element_blank())
}

#' Density Comparision Plot
#'
#' @param x
#'
#' numeric vector of sample data
#'
#' @param fits
#'
#' a list object produced from fit_univariate, fit_empirical, or
#' fit_univariate_man
#'
#' @param nbins
#'
#' number of bins for histogram
#'
#' @return
#'
#' ggplot of empirical histogram of x compared to theoretical density
#' distributions
#'
#' @export
#'
#' @examples
#' set.seed(37)
#' x <- rgamma(10000, 5)
#' dists <- c('gamma', 'lnorm', 'weibull')
#' fits <- lapply(dists, fit_univariate, x = x)
#' plot_density(x, fits, 30)
plot_density <- function(x, fits, nbins) {

  df <- data.frame(x = x)
  g <- ggplot(df, aes(x)) +
    geom_histogram(aes(y = ..density..),
                   bins = nbins,
                   fill = NA,
                   color = "black") +
    theme_bw() +
    theme(panel.grid = element_blank())

  ddists <- sapply(fits, function(fit) names(fit)[1])
  for (i in 1:length(fits)) {
    g <- g +
      stat_function(fun = fits[[i]][[1]],
                    aes_(color = ddists[i]),
                    size = 1)
  }
  g +
    scale_color_discrete(name = "distribution",
                         breaks = ddists)
}
