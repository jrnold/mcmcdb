\name{vcov-methods}
\alias{vcov,mcmc-method}
\alias{vcov,mcmc.list-method}
\alias{vcov-methods}
\title{Methods for function \code{vcov}}
\arguments{
  \item{object}{The MCMC sample object.}

  \item{...}{Options passed to \code{\link{cov}}.}
}
\description{
  Calculate the variance-covariance matrix for MCMC
  samples.
}
\section{Methods}{
  \describe{ \item{\code{signature(x="mcmc")}}{Method for
  class \code{mcmc}.}
  \item{\code{signature(x="mcmc.list")}}{Method for class
  \code{mcmc.list}.} }
}
\seealso{
  \code{\link[stats4:vcov-methods]{vcov}}
}

