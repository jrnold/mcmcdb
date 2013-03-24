\docType{methods}
\name{mcmcdb_fply-methods}
\alias{mcmcdb_fply}
\alias{mcmcdb_fply,McmcdbWide-method}
\alias{mcmcdb_fply-methods}
\title{Apply function to each flat parameter in MCMC samples}
\arguments{
  \item{x}{Object}

  \item{.fun}{function to apply to each iteration. The
  first argument of \code{.fun} will be an \code{array}; If
  the array has \code{m} dimensions, the first \code{m-1}
  dimensions are the dimensions of parameter array, and the
  last dimension is the iterations for all chains.}

  \item{...}{arguments passed to \code{\link{alply}} which
  is used internally.}
}
\value{
  \code{list}
}
\description{
  Split the MCMC samples into arrays of the original
  dimension x the total number of iterations and apply a
  function.
}
\seealso{
  Other McmcdbWide methods: \code{\link{$}},
  \code{\link{McmcdbWide-class}}, \code{\link{[}},
  \code{\link{[[}}, \code{\link{extract-methods}},
  \code{\link{mcmcdb_aply}},
  \code{\link{mcmcdb_aply-methods}},
  \code{\link{mcmcdb_chains}},
  \code{\link{mcmcdb_samples_flatpars}},
  \code{\link{mcmcdb_samples_flatpars}},
  \code{\link{mcmcdb_samples_long}},
  \code{\link{mcmcdb_samples_long}}
}

