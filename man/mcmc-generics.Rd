\name{coef}
\alias{coef}
\alias{mean}
\alias{mean,mcmc-method}
\alias{median}
\alias{median,mcmc-method}
\alias{quantile}
\alias{quantile,mcmc-method}
\alias{vcov}
\alias{vcov,mcmc-method}
\title{mcmc basic generic functions}
\arguments{
  \item{FUN}{Function used to calculate the point
  estimate.}
}
\description{
  This method is meant to be equivalent to the "coef"
  method for "mle" objects, in the sense that it returns a
  vector point estimate for each parameter. By default, the
  point estimate returned is the mean.
}

