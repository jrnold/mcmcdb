\docType{class}
\name{McmcPararray-class}
\alias{McmcPararray}
\alias{McmcPararray-class}
\title{McmcPararray Class}
\usage{
  McmcPararray(...)
}
\description{
  An S4 class containing metadata for an MCMC parameter
  array: its dimensions, and the names of the flattened
  parameters within it.
}
\section{Slots}{
  \describe{ \item{\code{flatpars}}{\code{"character"}.
  Name of flattened parameters in the parameter array}
  \item{\code{dim}}{\code{"integer"}. Dimension of the
  array} }
}
\keyword{internal}

