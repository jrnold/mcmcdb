\docType{class}
\name{McmcList2}
\alias{McmcList2}
\alias{McmcList2-class}
\title{List of MCMC chains with parameter metadata}
\description{
  This class extends the standard
  \code{\link[coda]{mcmc.list}} class with metadata about
  the shape of the parameters that can be used to convert
  them back to their original shapes to make postprocessing
  easier.
}
\section{Slots}{
  \describe{ \item{\code{parameters}}{Class
  \code{McmcParamterMeta}.} }
}

\section{Extends}{
  \describe{ \item{\code{mcmc.list}}{directly} }

  mcmc class with parameter metadata
}

\section{Extends}{
  \describe{
  \item{\code{parameters}}{\code{McmcParameterMeta} object}
  }
}
\keyword{classes}

