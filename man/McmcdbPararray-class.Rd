\docType{class}
\name{McmcdbPararray-class}
\alias{McmcdbPararray}
\alias{McmcdbPararray-class}
\title{McmcdbPararray Class}
\usage{
  McmcdbPararray(...)
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
\examples{
showClass("McmcdbPararray")
McmcdbPararray(flatpars=paste0("beta[", 1:2L, "]"), dim=2L)
}
\keyword{internal}

