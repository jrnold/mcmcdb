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

  \item{FUN}{\code{function}. Function to apply to each
  list of chains. The function should take the form
  \code{function(x)}, where \code{x} is a \code{list} of
  \code{array} objects.}

  \item{...}{Options passed to internal functions.}
}
\value{
  \code{list}. If \code{FUN = NULL}, each element of the
  list is a \code{list} of \code{array} objects,
  representing the sample values for each parameter array
  in each chain. If \code{FUN != NULL}, then each element
  contains the results of \code{FUN} for that parameter.
}
\description{
  Return MCMC samples as a list of parameter arrays Each
  parameter array is a list of arrays, containing the
  iterations in each chain.
}
\examples{
data(line_samples)
line_samples_pc <- mcmcdb_samples_pararrays_chain(line_samples)
str(line_samples_pc)
}
\seealso{
  Other Mcmcdb methods: \code{\link{mcmcdb_flatpars}},
  \code{\link{mcmcdb_flatpars}},
  \code{\link{mcmcdb_flatpars-methods}},
  \code{\link{mcmcdb_mcpar}}, \code{\link{mcmcdb_pardims}},
  \code{\link{mcmcdb_pardims}},
  \code{\link{mcmcdb_pardims-methods}},
  \code{\link{mcmcdb_paridx}}, \code{\link{mcmcdb_paridx}},
  \code{\link{mcmcdb_paridx-methods}},
  \code{\link{mcmcdb_samples_chain_flatpars}},
  \code{\link{mcmcdb_samples_chain_flatpars}},
  \code{\link{mcmcdb_samples_chain_iter}},
  \code{\link{mcmcdb_samples_chain_iter}},
  \code{\link{mcmcdb_samples_chain_pararrays}},
  \code{\link{mcmcdb_samples_chain_pararrays}},
  \code{\link{mcmcdb_samples_flatpars_chain}},
  \code{\link{mcmcdb_samples_flatpars_chain}},
  \code{\link{mcmdb_mcpar-methods}}, \code{\link{summary}},
  \code{\link{summary-methods}}
}

