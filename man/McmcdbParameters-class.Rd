\docType{class}
\name{McmcdbParameters-class}
\alias{McmcdbParameters}
\alias{McmcdbParameters-class}
\alias{dim,McmcdbParameters-method}
\alias{dimnames,McmcdbParameters-method}
\title{McmcdbParameters Class}
\usage{
  McmcdbParameters(...)
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
  \code{\link{McmcdbFlatparList}}. Names and information on
  the flattened parameters.} \item{\code{pararrays}}{An
  object of \code{\link{McmcdbPararrayList}}. Names and
  information on the parameter arrays.} }
}

\section{Methods}{
  \describe{ \item{dim}{\code{signature(x =
  "McmcdbParameters")}: Returns a named \code{integer}
  vector with the number of flat parameters and parameter
  arrays.} \item{dimnames}{\code{signature(x =
  "McmcdbParameters")}: Returns a \code{list} with names of
  the flat parameters and parameter arrays.} }
}
\examples{
showClass("McmcdbParameters")
}
\seealso{
  \code{\link{mcmc_parse_parnames}}

  Other McmcdbParameters methods:
  \code{\link{mcmcdb_flatpars}},
  \code{\link{mcmcdb_flatpars}},
  \code{\link{mcmcdb_flatpars-methods}},
  \code{\link{mcmcdb_par_indices}},
  \code{\link{mcmcdb_par_indices}},
  \code{\link{mcmcdb_par_indices-methods}},
  \code{\link{mcmcdb_pararrays}},
  \code{\link{mcmcdb_pararrays}},
  \code{\link{mcmcdb_pararrays-methods}},
  \code{\link{mcmcdb_pardims}},
  \code{\link{mcmcdb_pardims}},
  \code{\link{mcmcdb_pardims-methods}}
}
\keyword{internal}

