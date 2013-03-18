\name{mcmcdb_flatten-methods}
\alias{mcmcdb_flatten}
\alias{mcmcdb_flatten,array-method}
\alias{mcmcdb_flatten,numeric-method}
\alias{mcmcdb_flatten-methods}
\title{Flatten Array of MCMC samples to a vector}
\arguments{
  \item{x}{object}

  \item{parname}{\code{character} Parameter array name.}

  \item{FUN}{\code{function} Function with arguments
  \code{name} and \code{dim} and returns a vector of names
  to use.}
}
\value{
  Named \code{numeric} vector
}
\description{
  Flatten an array of MCMC samples to a numeric vector. As
  with R, the samples are flattened in column-major order.

  Currently these methods only handle objects representing
  a single iteration.
}

