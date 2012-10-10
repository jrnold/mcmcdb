\docType{methods}
\name{c-methods}
\alias{c,McmcList2-method}
\alias{c,McmcLong-method}
\alias{c-methods}
\title{Methods for function \code{c} for MCMC objects}
\arguments{
  \item{x}{Object to be concatenated}

  \item{...}{Other objects to be concatenated}
}
\description{
  These methods concatenate multiple objects of classes
  \code{McmcList2} and \code{McmcLong} into an object of
  the same class. This is generally used to combine results
  from several chains.
}
\keyword{methods}
