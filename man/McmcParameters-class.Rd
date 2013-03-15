\docType{class}
\name{McmcParameters-class}
\alias{McmcParameters}
\alias{McmcParameters-class}
\alias{dim,McmcParameters-method}
\alias{dimnames,McmcParameters-method}
\title{McmcParameters Class}
\usage{
  McmcParameters(...)
}
\description{
  Metadata about MCMC parameters, including the names of
  flattened parameters, mapping between flattened parameter
  names and parameter array names, and the dimenions of
  parameter arrays.
}
\details{
  Objects of this class are usually created by
  \code{\link{mcmc_parse_parnames}}.
}
\section{Slots}{
  \describe{ \item{\code{flatpars}}{An object of
  \code{\link{McmcFlatparList}}. Names and information on
  the flattened parameters.} \item{\code{pararrays}}{An
  object of \code{\link{McmcPararrayList}}. Names and
  information on the parameter arrays.} }
}

\section{Methods}{
  \describe{ \item{dim}{\code{signature(x =
  "McmcParameters")}: Returns a named \code{integer} vector
  with the number of flat parameters and parameter arrays.}
  \item{dimnames}{\code{signature(x = "McmcParameters")}:
  Returns a \code{list} with names of the flat parameters
  and parameter arrays.} }
}
\examples{
showClass("McmcParameters")
}
\seealso{
  \code{\link{mcmc_parse_parnames}}
}
\keyword{internal}

