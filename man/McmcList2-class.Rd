\name{McmcList2}
\alias{McmcList2}
\alias{McmcList2-class}
\title{Mcmc Samples Object with metadata about parameters}
\description{
  Mcmc Samples Object with metadata about parameters
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

