\name{McmcList2}
\alias{McmcList2}
\alias{McmcList2-class}
\title{Improved mcmc.list class}
\description{
  This class extends the standard
  \code{\link[coda]{mcmc.list}} class with metadata about
  the shape of the parameters that can be used to convert
  them back to their original shapes to make postprocessing
  easier.
}
\section{Slots}{
  \describe{ \item{\code{indices}}{List of matrices which
  map each flat parameter name to its parameter array.}
  \item{\code{template}}{List of parameter arrays in the
  same form as they were in the estimation.} }
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

