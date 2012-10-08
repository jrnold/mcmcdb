\name{quantile-methods}
\alias{quantile,mcmc-method}
\alias{quantile,mcmc.list-method}
\alias{quantile-methods}
\title{Quantile methods for MCMC samples}
\arguments{
  \item{x}{MCMC sample object.}

  \item{...}{Arguments passed to
  \code{\link[stats]{quantile}}.}
}
\description{
  Calculate quantile of paramters in MCMC sample objects.
}
\section{Methods}{
  \describe{ \item{\code{signature(x="mcmc")}}{Method for
  class \code{mcmc}.}
  \item{\code{signature(x="mcmc.list")}}{Method for class
  \code{mcmc.list}.} }
}

