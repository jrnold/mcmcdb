\docType{methods}
\name{mcmcdb_samples_flatpars}
\alias{mcmcdb_samples_flatpars}
\alias{mcmcdb_samples_flatpars,McmcdbWide-method}
\title{Extract MCMC Samples (Flat parameter form)}
\arguments{
  \item{object}{An object containing the MCMC samples.}

  \item{flatpars}{\code{character}. Flat parameters to
  include. If \code{NULL}, all flat parameters.}

  \item{parameters}{\code{character}. Parameter arrays to
  include. If \code{NULL}, all parameter arrays. The union
  of flat parameters in \code{parameters} and
  \code{flatpars} is included.}

  \item{chain_id}{\code{integer}. Chains to include. If
  \code{NULL}, all chains.}

  \item{iter}{\code{integer}. Iterations to include. If
  \code{NULL}, all iterations.}

  \item{FUN}{\code{function}. Function to apply to each}

  \item{...}{Options passed to internal functions.}
}
\value{
  If \code{FUN = NULL}, then a \code{matrix} with columns
  equal to flat parameters, and rows equal to iterations.
  If \code{FUN != NULL}, then a \code{list}, with the
  results of \code{FUN} applied to each flat parameter.
}
\description{
  Extract MCMC samples from an object as a a matrix of flat
  parameters.
}
\examples{
data(line_samples)
line_wide <- mcmcdb_samples_flatpars(line_samples)
dim(line_wide)
head(line_wide)
summary(line_wide)
}
\seealso{
  Other McmcdbWide methods: \code{\link{$}},
  \code{\link{$-methods}}, \code{\link{McmcdbWide-class}},
  \code{\link{[}}, \code{\link{[-methods}},
  \code{\link{[[}}, \code{\link{[[-methods}},
  \code{\link{mcmcdb_chains}},
  \code{\link{mcmcdb_drop_chain}},
  \code{\link{mcmcdb_drop_chain}},
  \code{\link{mcmcdb_drop_chain}},
  \code{\link{mcmcdb_drop_iters}},
  \code{\link{mcmcdb_drop_iters}},
  \code{\link{mcmcdb_drop_iters}},
  \code{\link{mcmcdb_drop_parameter}},
  \code{\link{mcmcdb_drop_parameter}},
  \code{\link{mcmcdb_samples_iter}},
  \code{\link{mcmcdb_samples_iter}},
  \code{\link{mcmcdb_samples_long}},
  \code{\link{mcmcdb_samples_long}},
  \code{\link{mcmcdb_samples_parameters}},
  \code{\link{mcmcdb_samples_parameters}},
  \code{\link{mcmcdb_unflatten}},
  \code{\link{mcmcdb_unflatten}},
  \code{\link{mcmcdb_unflatten}},
  \code{\link{mcmcdb_unflatten}},
  \code{\link{mcmcdb_unflatten}},
  \code{\link{mcmcdb_unflatten}},
  \code{\link{mcmcdb_unflatten}},
  \code{\link{mcmcdb_unflatten}},
  \code{\link{mcmcdb_unflatten-method}}
}

