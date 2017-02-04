build_dist <- function(x, family) {

  # generate list of distribution functions
  type <- c('d', 'p', 'q', 'r')
  funs <- lapply(type, function(type) {
    get(paste0(type, family))
  })
  names(funs) <- type

  params <- fitdistrplus::fitdist(data = x, distr = family)[['estimate']]
  lapply(setNames(funs, names(funs)), gen_dist_fun,
         parameters = params)
}

gen_dist_fun <- function(f, parameters, ...) {
  function(...)
    do.call(f, c(list(...), parameters))
}

fittedDists <- build_dist(rpois(100,5), 'pois')
dpois(x = 5, lambda = 5)
fittedDists$d(5)
ppois(5, 5)
fittedDists$p(5)
qpois(.5, 5)
fittedDists$q(.5)
set.seed(82)
rpois(100, 5)
set.seed(82)
fittedDists$r(100)
