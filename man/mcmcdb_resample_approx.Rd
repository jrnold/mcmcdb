\docType{methods}
\name{mcmcdb_resample_approx}
\alias{mcmcdb_resample_approx}
\alias{mcmcdb_resample_approx-methods}
\title{Resample from a Posterior Approximation}
\arguments{
  \item{object}{object with flat parameters.}

  \item{n}{Number of samples to draw.}

  \item{scale_mult}{Multiplier of scale matrix}

  \item{df}{Degrees of freedom. If \code{Inf}, then a
  normal distribution is used. Otherwise, a Student t
  distribution is used.}

  \item{...}{Passed to
  \code{\link{mcmcdb_samples_flatpars}}.}
}
\description{
  Draw samples from a normal or t-distribution
  approximation of the posterior distribution.
}

