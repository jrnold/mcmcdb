\docType{methods}
\name{mcmcSkeleton}
\alias{mcmcIndices,McmcLong-method}
\alias{mcmcIndices,McmcParnames-method}
\alias{mcmcIndices,data.frame-method}
\alias{mcmcIndices-methods}
\alias{mcmcSkeleton}
\alias{mcmcSkeleton,McmcLong-method}
\alias{mcmcSkeleton,McmcPararrays-method}
\alias{mcmcSkeleton,data.frame-method}
\alias{mcmcSkeleton-methods}
\title{Create Indices for MCMC Sample parameters}
\arguments{
  \item{x}{object}

  \item{x}{\code{McmcPararrays} object}
}
\value{
  Named \code{list} of matrices. Each element is the
  indices of the unlisted parameters in the parameter
  arrays.

  \code{list} of numeric \code{array} objects, one for each
  value of \code{pararray}, and in the dimension of that
  \code{parrray}, but with all entries set to 0.
}
\description{
  Create Indices for MCMC Sample parameters

  Create skeleton for relisting MCMC samples
}
\keyword{methods}

