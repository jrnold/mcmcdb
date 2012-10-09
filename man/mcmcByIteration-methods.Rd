\docType{methods}
\name{mcmcByIteration-methods}
\alias{mcmcByIteration}
\alias{mcmcByIteration,McmcList2-method}
\alias{mcmcByIteration,McmcLong-method}
\alias{mcmcByIteration,McmcWide-method}
\alias{mcmcByIteration-methods}
\title{Iterate over all Mcmc iterations}
\arguments{
  \item{object}{\code{McmcList2} object.}

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

