#' Fit Univariate Distribution
#'
#' @param x
#'
#' numeric vector
#'
#' @param distribution
#'
#' character name of distribution
#'
#' @param type
#'
#' discrete or continuous data
#'
#' @return
#'
#' a fitted list object of d, p, q, r distribution functions and parameters,
#' MLE for probability distributions, custom fit for empirical
#'
#' @export
#'
#' @examples
#' # Fit Discrete Distribution
#' set.seed(42)
#' x <- rpois(1000, 3)
#' fitted <- fit_univariate(x, 'pois', type = 'discrete')
#' # density function
#' plot(fitted$dpois(x=0:10),
#'      xlab = 'x',
#'      ylab = 'dpois')
#' # distribution function
#' plot(fitted$ppois(seq(0, 10, 1)),
#'      xlab= 'x',
#'      ylab = 'ppois')
#' # quantile function
#' plot(fitted$qpois,
#'      xlab= 'x',
#'      ylab = 'qpois')
#' # sample from theoretical distribution
#' summary(fitted$rpois(100))
#' # estimated parameters from MLE
#' fitted$parameters
#'
#' set.seed(24)
#' x <- rweibull(1000, shape = .5, scale = 2)
#' fitted <- fit_univariate(x, 'weibull')
#' # density function
#' plot(fitted$dweibull,
#'      xlab = 'x',
#'      ylab = 'dweibull')
#' # distribution function
#' plot(fitted$pweibull,
#'      xlab = 'x',
#'      ylab = 'pweibull')
#' # quantile function
#' plot(fitted$qweibull,
#'      xlab = 'x',
#'      ylab = 'qweibull')
#' # sample from theoretical distribution
#' summary(fitted$rweibull(100))
#' # estimated parameters from MLE
#' fitted$parameters
fit_univariate <- function(x, distribution, type = 'continuous') {

  stopifnot(type == 'discrete' & is.integer(x) |
            type == 'continuous' & is.double(x) |
            type == 'empirical' & is.numeric(x))
  discreteDists <- c('geom', 'nbinom', 'pois', 'dunif')
  continuousDists <- c('exp', 'cauchy', 'gamma', 'lnorm',
                     'norm', 'unif', 'weibull', 'llogis', 'logis',
                     'invweibull', 'invgamma')
  if (!distribution %in% c(discreteDists, continuousDists, 'empirical')) {
    stop("distribution not in supported distributions")
  }
  if (distribution %in% 'empirical') {
    fit_empirical(x)
  } else {
    build_dist(x, distribution)
  }

}
# use structure to turn into class "distfun"
# think about creating hierarchiny class "distsfun"

#' Build Distribution Functions
#'
#' A wrapper for building function families given
#' a numeric vector and the distribution
#'
#' @param x
#'
#' numeric vector
#'
#' @param distribution
#'
#' distribution character name
#'
#' @return list of distribution functions for d, p, q, r, and parameters
#'
#' @rawNamespace import(actuar, except = c(sd, var))
#'
#' @export
#'
#' @examples
#' fittedDists <- build_dist(rpois(100,5), 'pois')
#' dpois(x = 5, lambda = 5)
#' fittedDists$dpois(5)
#' ppois(5, 5)
#' fittedDists$ppois(5)
#' qpois(.5, 5)
#' fittedDists$qpois(.5)
#' set.seed(8257)
#' rpois(100, 5)
#' set.seed(8257)
#' fittedDists$rpois(100)
#' fittedDists$parameters


build_dist <- function(x, distribution) {
  # generate list of distribution functions
  type <- paste0(c('d', 'p', 'q', 'r'), distribution)
  funs <- lapply(type, function(type) {
    match.fun(type)
  })
  names(funs) <- type

  if (distribution %in% 'dunif') {
    parameters <- c(min = min(x), max = max(x))
  } else {
    parameters <- fitdistrplus::fitdist(data = x, distr = distribution)[['estimate']]
  }
  funs <- lapply(stats::setNames(funs, names(funs)), gen_dist_fun,
                 parameters = parameters)
  funs[['parameters']] <- parameters
  structure(funs,
            class = "distfun")
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

  funs <- lapply(stats::setNames(funs, names(funs)),
                 gen_dist_fun,
                 parameters = parameters)
  funs[['parameters']] <- parameters
  structure(funs,
            class = "distfun")

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
#'
#' @return
#' one of parameterized distribution functions in d, p, q, r

gen_dist_fun <- function(f, parameters, ...) {
  function(...)
    do.call(f, c(list(...), parameters))
}

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
    if (p < 0 | p > 1) {
      warning("NaNs produced", call. = FALSE)
      NaN
    } else {
      max(values[1], values[cumsum(probs) <= p])
    }
  })

  r <- function(n) {
    sample(x = values, size = n, prob = probs, replace = TRUE)
  }
  structure(list(dempDis = d,
                 pempDis = p,
                 qempDis = q,
                 rempDis = r,
                 parameters = probs),
            class = "distfun")
}

fit_empirical_continuous <- function(x) {
  stopifnot(is.double(x))
  x <- sort(x)
  # Freedman-Diaconis rule for optimal number of histogram bins
  nbins <- diff(range(x)) / (2 * stats::IQR(x) / length(x)^(1/3))
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
    if (p < 0 | p > 1) {
      warning("NaNs produced", call. = FALSE)
      NaN
    } else {
      max(mids[1], mids[cumsum(probs) <= p])
    }
  })

  r <- function(n) {
    sample(x = mids, size = n, prob = probs, replace = TRUE)
  }
  structure(list(dempCont = d,
                 pempCont = p,
                 qempCont = q,
                 rempCont= r,
                 parameters = probs),
            class = "distfun")
}

get_interval_nums <- function(cuts) {
  numChars <- strsplit(gsub('\\(|\\]|\\)|\\]', "", cuts), ",")
  lapply(numChars, as.numeric)
}
