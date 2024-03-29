% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/discrete_uniform.R
\name{DiscreteUniform}
\alias{DiscreteUniform}
\alias{ddunif}
\alias{pdunif}
\alias{qdunif}
\alias{rdunif}
\title{The Discrete Uniform Distribution}
\usage{
ddunif(x, min = 0, max = 1)

pdunif(q, min = 0, max = 1)

qdunif(p, min = 0, max = 1)

rdunif(n, min = 0L, max = 1)
}
\arguments{
\item{x}{vector of (non-negative integer) quantiles}

\item{min}{minimum value of distribution (integer)}

\item{max}{maximum value of distribution (integer)}

\item{q}{vector of quantiles}

\item{p}{vector of probabilities}

\item{n}{number of random values to return}
}
\value{
ddunif gives the density, pdunif gives the distribution function,
qdunif gives the quantile function, rdunif generates random deviates
}
\description{
The Discrete Uniform Distribution
}
\examples{
ddunif(0:1)
pdunif(1)
qdunif(.5)
rdunif(10)
}
