\name{mcmc_relist}
\alias{mcmc_relist}
\title{Relist MCMC samples}
\usage{
  mcmc_relist(flesh, skeleton, indices)
}
\arguments{
  \item{flesh}{\code{numeric}}

  \item{skeleton}{Named \code{list} of arrays of zeros in
  the shape of each parameter array.}

  \item{indices}{Named \code{list} of matrices with the
  indices}
}
\value{
  \code{list} of arrays in the same shape as
  \code{skeleton} filled in with the parameter values.
}
\description{
  Relist MCMC parameters from their unlisted form into a
  list of arrays in the origin See \code{\link{mcmcRelist}}
  for a method.
}

