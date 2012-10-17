\docType{methods}
\name{mcmcByIteration-methods}
\alias{mcmcByIteration}
\alias{mcmcByIteration-methods}
\alias{mcmcByIteration,McmcLong-method}
\title{Iterate over all Mcmc iterations}
\arguments{
  \item{data}{\code{function} Data to combine with
  parameters on each iteration.}

  \item{FUN}{\code{function} Function to apply to each
  iteration.}

  \item{...}{Pass to \code{FUN}.}
}
\description{
  Iterate over all Mcmc iterations
}
\section{Methods}{
  \describe{
  \item{\code{signature(object="mcmcList2")}}{Method for
  class \code{mcmcList}.}
  \item{\code{signature(object="mcmcLong"}}{Method for
  class \code{mcmcLong}.}
  \item{\code{signature(object="mcmcWide"}}{Method for
  class \code{mcmcWide}.} }
}
\keyword{methods}

