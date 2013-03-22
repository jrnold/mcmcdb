\docType{data}
\name{line_samples}
\alias{line_samples}
\title{Line MCMC samples}
\format{An object of class \code{"McmcdbWide"} with
2 parameter arrays, \code{beta} (length 2) and \code{tau} (length 1),
and 2 chains of 100 samples each.}
\description{
  A \code{"McmcdbWide"} object used for examples in this
  package.
}
\examples{
data(line_samples)
show(line_samples)
mcmcdb_flatpars(line_samples)
mcmcdb_mcpar(line_samples)
}
\keyword{datasets}
