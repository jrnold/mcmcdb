\docType{methods}
\name{mcmcdb_samples_chain_flatpars}
\alias{mcmcdb_samples_chain_flatpars}
\alias{mcmcdb_samples_chain_flatpars,Mcmcdb-method}
\title{Extract MCMC samples (Chains, Flat parameters)}
\arguments{
  \item{object}{An object containing the MCMC samples.}

  \item{flatpars}{\code{character}. Flat parameters to
  include. If \code{NULL}, all flat parameters.}

  \item{pararrays}{\code{character}. Parameter arrays to
  include. If \code{NULL}, all parameter arrays.}

  \item{chain_id}{\code{integer}. Chains to include. If
  \code{NULL}, all chains.}

  \item{iter}{\code{integer}. Iterations to include. If
  \code{NULL}, all iterations.}

  \item{...}{Options passed to internal functions.}
}
\value{
  \code{list}. Each element is a chain. Each chain is a
  named \code{list} of \code{matrix} objects. Each matrix
  contains the iterations of the flat parameters.
}
\description{
  Return MCMC samples as a list of chains. Each chain is a
  named list of arrays with each array containing all
  iterations in that chain for that parameter array.
}
\examples{
data(line_samples)
line_samples_chain_pars <- mcmcdb_samples_chain_flatpars(line_samples)
length(line_samples_chain_pars)
summary(line_samples_chain_pars[[1]])
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
  \code{\link{mcmcdb_samples_chain_iter}},
  \code{\link{mcmcdb_samples_chain_iter}},
  \code{\link{mcmcdb_samples_chain_pararrays}},
  \code{\link{mcmcdb_samples_chain_pararrays}},
  \code{\link{mcmcdb_samples_flatpars_chain}},
  \code{\link{mcmcdb_samples_flatpars_chain}},
  \code{\link{mcmcdb_samples_pararrays_chain}},
  \code{\link{mcmcdb_samples_pararrays_chain}},
  \code{\link{mcmdb_mcpar-methods}}, \code{\link{summary}},
  \code{\link{summary-methods}}
}

