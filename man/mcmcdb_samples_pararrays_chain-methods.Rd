\docType{methods}
\name{mcmcdb_samples_pararrays_chain}
\alias{mcmcdb_samples_pararrays_chain}
\alias{mcmcdb_samples_pararrays_chain,Mcmcdb-method}
\title{Extract MCMC samples (Parameter arrays, chains)}
\arguments{
  \item{object}{An object containing the MCMC samples.}

  \item{pararrays}{\code{character}. Parameter arrays to
  include. If \code{NULL}, all parameter arrays.}

  \item{chain_id}{\code{integer}. Chains to include. If
  \code{NULL}, all chains.}

  \item{iter}{\code{integer}. Iterations to include. If
  \code{NULL}, all iterations.}

  \item{...}{Options passed to internal functions.}
}
\value{
  \code{list}. Each element of the list is a \code{list} of
  \code{array} objects, representing the samples of each
  parameter array in each chain.
}
\description{
  Return MCMC samples as a list of paramter arrays Each
  paramter array is a list of arrays, containing the
  iterations in each chain.
}
\examples{
data(line_samples)
line_samples_pc <- mcmcdb_samples_pararrays_chain(line_samples)
str(line_samples_pc)
}
\seealso{
  Other McmcdbWide methods: \code{\link{$}},
  \code{\link{$-methods}}, \code{\link{McmcdbWide-class}},
  \code{\link{[}}, \code{\link{[-methods}},
  \code{\link{[[}}, \code{\link{[[-methods}},
  \code{\link{mcmcdb_chains}},
  \code{\link{mcmcdb_samples_chain_flatpars}},
  \code{\link{mcmcdb_samples_chain_flatpars}},
  \code{\link{mcmcdb_samples_chain_iter}},
  \code{\link{mcmcdb_samples_chain_iter}},
  \code{\link{mcmcdb_samples_chain_pararrays}},
  \code{\link{mcmcdb_samples_chain_pararrays}},
  \code{\link{mcmcdb_samples_flatpars}},
  \code{\link{mcmcdb_samples_flatpars}},
  \code{\link{mcmcdb_samples_flatpars_chain}},
  \code{\link{mcmcdb_samples_flatpars_chain}},
  \code{\link{mcmcdb_samples_iter}},
  \code{\link{mcmcdb_samples_iter}},
  \code{\link{mcmcdb_samples_long}},
  \code{\link{mcmcdb_samples_long}},
  \code{\link{mcmcdb_samples_pararrays}},
  \code{\link{mcmcdb_samples_pararrays}}
}

