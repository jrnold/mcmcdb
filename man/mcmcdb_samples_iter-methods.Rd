\docType{methods}
\name{mcmcdb_samples_iter}
\alias{mcmcdb_samples_iter}
\alias{mcmcdb_samples_iter,McmcdbWide-method}
\title{Extract MCMC samples (Iterations)}
\arguments{
  \item{object}{An object containing the MCMC samples.}

  \item{pararrays}{\code{character}. Parameter arrays to
  include. If \code{NULL}, all parameter arrays.}

  \item{chain_id}{\code{integer}. Chains to include. If
  \code{NULL}, all chains.}

  \item{iter}{\code{integer}. Iterations to include. If
  \code{NULL}, all iterations.}

  \item{FUN}{\code{function}. Function to apply to each
  iteration. \code{function(x)}, where \code{x} is a named
  \code{list} of the parameter arrays for a single
  iteration.}

  \item{...}{Options passed to internal functions.}
}
\value{
  \code{list}. If \code{FUN = NULL}, then each element of
  the list is a \code{list} of \code{array} objects
  representing the parameter arrays. If \code{FUN != NULL},
  then each element is the result of \code{FUN} applied to
  the iteration.
}
\description{
  Return MCMC samples as a list of iterations, in which
  each iteration is a list of parameter arrays.
}
\examples{
data(line_samples)
line_samples_iter <- mcmcdb_samples_iter(line_samples)
length(line_samples_iter)
line_samples_iter[[1]]
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
  \code{\link{mcmcdb_drop_pararray}},
  \code{\link{mcmcdb_drop_pararray}},
  \code{\link{mcmcdb_samples_flatpars}},
  \code{\link{mcmcdb_samples_flatpars}},
  \code{\link{mcmcdb_samples_long}},
  \code{\link{mcmcdb_samples_long}},
  \code{\link{mcmcdb_samples_pararrays}},
  \code{\link{mcmcdb_samples_pararrays}}
}

