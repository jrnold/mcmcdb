\docType{methods}
\name{mcmcdb_aply-methods}
\alias{mcmcdb_aply}
\alias{mcmcdb_aply,McmcdbWide-method}
\alias{mcmcdb_aply-methods}
\title{Apply function to each parameter array in MCMC samples}
\arguments{
  \item{x}{Object}

  \item{.fun}{function to apply to each flat parameter. The
  first argument of \code{.fun} will be a \code{numeric}
  vector repres}

  \item{...}{arguments passed to \code{\link{alply}} which
  is used internally.}
}
\value{
  \code{list}
}
\description{
  Split the MCMC samples into flat paramers, vectors of
  length equal to the total number of iterations, and apply
  a function to each.
}
\seealso{
  Other McmcdbWide methods: \code{\link{$}},
  \code{\link{McmcdbWide-class}}, \code{\link{[}},
  \code{\link{[[}}, \code{\link{extract-methods}},
  \code{\link{mcmcdb_chains}},
  \code{\link{mcmcdb_flatpars}},
  \code{\link{mcmcdb_flatpars}},
  \code{\link{mcmcdb_flatpars-methods}},
  \code{\link{mcmcdb_fply}},
  \code{\link{mcmcdb_fply-methods}},
  \code{\link{mcmcdb_iters}}, \code{\link{mcmcdb_mcpar}},
  \code{\link{mcmcdb_metadata}},
  \code{\link{mcmcdb_parameters}},
  \code{\link{mcmcdb_parameters-methods}},
  \code{\link{mcmcdb_pardims}},
  \code{\link{mcmcdb_pardims}},
  \code{\link{mcmcdb_pardims-methods}},
  \code{\link{mcmdb_mcpar-methods}}
}

