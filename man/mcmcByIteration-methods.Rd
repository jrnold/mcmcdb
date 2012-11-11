\docType{methods}
\name{mcmcByIteration-methods}
\alias{mcmcByIteration}
\alias{mcmcByIteration,McmcLong-method}
\alias{mcmcByIteration-methods}
\title{Iterate over all Mcmc iterations}
\arguments{
  \item{data}{\code{list} Data to combine with parameters
  on each iteration.}

  \item{.fun}{\code{function} Function to apply to each
  iteration. This should take one argument, a \code{list}
  that includes the parameters in array form and the values
  from \code{data}.}

  \item{...}{Options passed to \code{.fun} and
  \code{\link{dlply}}.}
}
\description{
  Iterate over all Mcmc iterations
}
\keyword{methods}

