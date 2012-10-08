\name{coef-methods}
\alias{coef,mcmc-method}
\alias{coef,mcmc.list-method.}
\alias{coef-methods}
\title{Methods for function \code{mean}}
\arguments{
  \item{FUN}{Function used to calculate the point
  estimate.}
}
\description{
  This method is meant to be equivalent to the \code{coef}
  method for \code{mle} objects in package \pkg{stats4}, in
  the sense that it returns a vector point estimate for
  each parameter. By default, the point estimate returned
  is the mean.
}
\section{Methods}{
  \describe{ \item{\code{signature(object="mcmc")}}{Method
  for class \code{mcmc}}
  \item{\code{signature(object="mcmc.list")}}{Method for
  class \code{mcmc.list}} }
}

