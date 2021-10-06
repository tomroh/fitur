#' Calculate moments of a numeric vector
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
#' calc_moments(x)
calc_moments <- function(x) {
  stopifnot(is.numeric(x))
  c(mean = mean(x),
    sd = stats::sd(x),
    skewness = skewness(x),
    kurtosis = kurtosis(x))
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
#' library(ggplot2)
#' set.seed(37)
#' x <- rgamma(10000, 5)
#' dists <- c('gamma', 'lnorm', 'weibull')
#' fits <- lapply(dists, fit_univariate, x = x)
#' plot_qq(x, fits) +
#' theme_bw()
plot_qq <- function(x, fits) {
  stopifnot(is.distfun(fits) | all(sapply(fits, is.distfun)))
  if ( is.distfun(fits) ) fits <- list(fits)
  theorQuant <- lapply(fits, function(fit) {
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
#' library(ggplot2)
#' set.seed(37)
#' x <- rgamma(10000, 5)
#' dists <- c('gamma', 'lnorm', 'weibull')
#' fits <- lapply(dists, fit_univariate, x = x)
#' plot_pp(x, fits) +
#' theme_bw()
plot_pp <- function(x, fits) {
  stopifnot(is.distfun(fits) | all(sapply(fits, is.distfun)))
  if ( is.distfun(fits) ) fits <- list(fits)
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
    theme(panel.grid = element_blank())
}

#' Density Comparison Plot
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
#' library(ggplot2)
#' set.seed(37)
#' x <- rgamma(10000, 5)
#' dists <- c('gamma', 'lnorm', 'weibull')
#' fits <- lapply(dists, fit_univariate, x = x)
#' plot_density(x, fits, 30) +
#' theme_bw()
plot_density <- function(x, fits, nbins) {
  stopifnot(is.distfun(fits) | all(sapply(fits, is.distfun)))
  if ( is.distfun(fits) ) fits <- list(fits)
  df <- data.frame(x = x)
  g <- ggplot(df, aes(x)) +
    geom_histogram(aes(y = ..density..),
                   bins = nbins,
                   fill = NA,
                   color = "black") +
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

#' Wrappers to compute goodness of fit test froms distfun objects
#'
#' @param distfun
#'
#' a distfun object
#'
#' @param x
#'
#' numeric vector
#'
#' @param ...
#'
#' arguments to be passed on to test function
#'
#' @return
#'
#' goodness of fit object
#'
#' @export
#'
#' @name GOFTests
#'
#' @examples
#' x <- rgamma(100, 1, 1)
#' fit <- fit_univariate(x, 'gamma')
#' ks_test(fit, x)
#' ad_test(fit, x)
#' cvm_test(fit, x)
ks_test <- function(distfun, x, ...) {
  UseMethod("ks_test")
}

#' @export
ks_test.distfun <- function(distfun, x, ...) {
  stats::ks.test(x, distfun[[2]], ...)
}

#' @rdname GOFTests
#'
#' @importFrom goftest ad.test
#'
#' @export
ad_test.distfun <- function(distfun, x) {
  ad.test(x, null = distfun[[2]])
}

#' @rdname GOFTests
#' @export
ad_test <- function(distfun, x) {
  UseMethod("ad_test")
}

#' @rdname GOFTests
#'
#' @importFrom goftest cvm.test
#'
#' @export
cvm_test.distfun <- function(distfun, x) {
  cvm.test(x, null = distfun[[2]])
}

#' @rdname GOFTests
#' @export
cvm_test <- function(distfun, x) {
  UseMethod("cvm_test")
}

#' Goodness of Fit Testing
#'
#' Apply all goodness of fit tests and return a data.frame with the results
#'
#' @param fits
#'
#' a list object produced from fit_univariate, fit_empirical, or
#' fit_univariate_man
#'
#' @param x
#'
#' numeric vector of sample data
#'
#' @return
#'
#' a data.frame of test statistic results for each distribution
#'
#' @export
#'
#' @examples
#' set.seed(84)
#' x <- rgamma(100, 1, 1)
#' dists <- c('gamma', 'lnorm', 'weibull')
#' multipleFits <- lapply(dists, fit_univariate, x = x)
#' gof_tests(multipleFits, x)
gof_tests <- function(fits, x) {
  stopifnot(is.distfun(fits) | all(sapply(fits, is.distfun)))
  gofTests <- stats::setNames(c(ks_test, ad_test, cvm_test),
                       c("ks", "ad", "cv"))
  if ( is.distfun(fits) ) fits <- list(fits)
  names(fits) <- sapply(fits, function(fit) sub("^d", "", names(fit)[[1]]))
  tests <- lapply(fits, function(fit) {
    lapply(gofTests, gof_test, fit = fit, x = x)
  })
  tests <- cbind(distribution = names(tests),
                 do.call(rbind, lapply(tests, as.data.frame)),
                 stringsAsFactors = FALSE,
                 row.names = NULL)
  stats::setNames(tests, gsub("\\.", "_", names(tests)))
}

gof_test <- function(fit, gofTest, x = x) {
  data.frame(unclass(gofTest(fit, x)[c('statistic', 'p.value')]))
}

#' Test if object is a distfun object
#'
#' @param x
#'
#' an R object to be tested
#'
#' @return
#'
#' TRUE if x is a disfun object, FALSE otherwise
#'
#' @export
#'
is.distfun <- function(x) {
  inherits(x, "distfun")
}


