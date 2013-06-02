\docType{methods}
\name{mcmcdb_samples_parameters}
\alias{mcmcdb_samples_parameters}
\alias{mcmcdb_samples_parameters,McmcdbWide-method}
\title{Extract MCMC samples (Parameter arrays)}
\arguments{
  \item{object}{An object containing the MCMC samples.}

  \item{parameters}{\code{character}. Parameter arrays to
  include. If \code{NULL}, all parameter arrays.}

  \item{chain_id}{\code{integer}. Chains to include. If
  \code{NULL}, all chains.}

  \item{iter}{\code{integer}. Iterations to include. If
  \code{NULL}, all iterations.}

  \item{FUN}{\code{function}. Function to apply to each
  paramter array. \code{function(x)} where \code{x} is the}

  \item{...}{Options passed to internal functions.}
}
\value{
  \code{list} of \code{array} objects. The arrays represent
  all iterations of each parameter array.
}
\description{
  Extract MCMC samples from an object as a a list of
  arrays.
}
\examples{
data(line_samples)
line_arrays <- mcmcdb_samples_parameters(line_samples)
summary(line_arrays)
lapply(line_arrays, dim)
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
  \code{\link{mcmcdb_samples_flatpars}},
  \code{\link{mcmcdb_samples_flatpars}},
  \code{\link{mcmcdb_samples_iter}},
  \code{\link{mcmcdb_samples_iter}},
  \code{\link{mcmcdb_samples_long}},
  \code{\link{mcmcdb_samples_long}},
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

