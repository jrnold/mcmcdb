\docType{methods}
\name{mcmcdb_iply-methods}
\alias{mcmcdb_iply}
\alias{mcmcdb_iply,McmcdbWide-method}
\alias{mcmcdb_iply-methods}
\title{Apply function to each iteration of MCMC samples}
\arguments{
  \item{x}{Object}

  \item{.fun}{function to apply to each iteration. The
  first argument of \code{.fun} will be the list of
  parameter arrays for that iteration.}

  \item{...}{arguments passed to \code{\link{alply}} which
  is used internally.}
}
\value{
  \code{list}
}
\description{
  Split the MCMC samples into iterations, convert the
  parameters into a named list of arrays, and apply a
  function to each iteration.
}
\seealso{
  Other McmdbWide methods: \code{\link{mcmcdb_unflatten}},
  \code{\link{mcmcdb_unflatten}},
  \code{\link{mcmcdb_unflatten}},
  \code{\link{mcmcdb_unflatten}},
  \code{\link{mcmcdb_unflatten}},
  \code{\link{mcmcdb_unflatten}},
  \code{\link{mcmcdb_unflatten}},
  \code{\link{mcmcdb_unflatten}},
  \code{\link{mcmcdb_unflatten-method}}
}

