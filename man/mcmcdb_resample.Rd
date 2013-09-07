\docType{methods}
\name{mcmcdb_resample}
\alias{mcmcdb_resample}
\alias{mcmcdb_resample,Mcmcdb-method}
\alias{mcmcdb_resample-methods}
\title{Resample from a MCMC Samples}
\arguments{
  \item{object}{object with flat parameters.}

  \item{n}{Number of samples to draw.}

  \item{replace}{Sample with replacement?}

  \item{...}{Passed to
  \code{\link{mcmcdb_samples_flatpars}}.}
}
\description{
  Draw samples from a normal or t-distribution
  approximation of the posterior distribution.
}

