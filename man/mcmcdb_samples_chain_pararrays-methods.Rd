\docType{methods}
\name{mcmcdb_samples_chain_pararrays}
\alias{mcmcdb_samples_chain_pararrays}
\alias{mcmcdb_samples_chain_pararrays,Mcmcdb-method}
\title{Extract MCMC Samples (Chains, Paramter arrays)}
\arguments{
  \item{object}{An object containing the MCMC samples.}

  \item{pararrays}{\code{character}. Parameter arrays to
  include.}

  \item{chain_id}{\code{integer}. Chains to include. If
  \code{NULL}, all chains.}

  \item{iter}{\code{integer}. Iterations to include. If
  \code{NULL}, all iterations.}

  \item{...}{Options passed to internal functions.}
}
\value{
  \code{list}. Each element represents a chain. Each
  element of the chain is a named \code{list} of arrays,
  each array representing a parameter array and including
  the iterations of that parameter.
}
\description{
  Extract MCMC samples as a list of chains, Each chain is a
  named list of parameter arrays.
}
\examples{
data(line_samples)
line_samples_chain_pars <- mcmcdb_samples_chain_pararrays(line_samples)
lapply(line_samples_chain_pars[[1]], dim)
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
  \code{\link{mcmcdb_samples_flatpars_chain}},
  \code{\link{mcmcdb_samples_flatpars_chain}},
  \code{\link{mcmcdb_samples_pararrays_chain}},
  \code{\link{mcmcdb_samples_pararrays_chain}},
  \code{\link{mcmdb_mcpar-methods}}, \code{\link{summary}},
  \code{\link{summary-methods}}
}

