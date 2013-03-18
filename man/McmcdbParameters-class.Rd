\docType{class}
\name{McmcdbParameters-class}
\alias{McmcdbParameters-class}
\alias{dim,McmcdbParameters-method}
\alias{dimnames,McmcdbParameters-method}
\title{McmcdbParameters Class}
\description{
  Metadata about MCMC parameters, providing information
  about their dimension and the names of the associated
  flattened parameters. This class and its methods are used
  for converting between parameter arrays and flattened
  arrays.

  Objects of this class are usually created by the
  \code{\link{McmcdbParameters}} function.

  \describe{ \item{\code{ListOfCharArrays}}{directly. A
  \code{list} in which all elements must be arrays
  containing \code{character} data.}
  \item{\code{namedList}}{By \code{ListOfCharArrays}} }
}
\section{Methods}{
  \describe{ \item{dim}{\code{signature(x =
  "McmcdbParameters")}: Returns a named \code{list} of
  \code{integer} vectors with the dimensions of each
  parameter array.} \item{dimnames}{\code{signature(x =
  "McmcdbParameters")}: Returns a named \code{list} of
  \code{character} vectors with names of the flat
  parameters in each array.} }
}
\examples{
showClass("McmcdbParameters")
}
\seealso{
  \code{\link{McmcdbParameters}}

  Other McmcdbParameters.methods:
  \code{\link{mcmcdb_flatpars}},
  \code{\link{mcmcdb_flatpars-methods}},
  \code{\link{mcmcdb_pardims}},
  \code{\link{mcmcdb_pardims-methods}},
  \code{\link{mcmcdb_paridx}},
  \code{\link{mcmcdb_paridx-methods}}
}
\keyword{internal}

