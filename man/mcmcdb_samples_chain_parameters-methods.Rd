\docType{methods}
\name{mcmcdb_samples_chain_parameters}
\alias{mcmcdb_samples_chain_parameters}
\alias{mcmcdb_samples_chain_parameters,Mcmcdb-method}
\title{Extract MCMC Samples (Chains, Paramter arrays)}
\arguments{
  \item{object}{An object containing the MCMC samples.}

  \item{parameters}{\code{character}. Parameter arrays to
  include.}

  \item{chain_id}{\code{integer}. Chains to include. If
  \code{NULL}, all chains.}

  \item{iter}{\code{integer}. Iterations to include. If
  \code{NULL}, all iterations.}

  \item{FUN}{\code{function} Function to apply to each
  chain's list of parameter arrays. \code{function(x)}
  where \code{x} is a named \code{list} of \code{array}
  objects.}

  \item{...}{Options passed to internal functions.}
}
\value{
  \code{list}. If \code{FUN = NULL}, then each element
  represents a chain. Each element of the chain is a named
  \code{list} of arrays, each array representing a
  parameter array and including the iterations of that
  parameter. If \code{FUN != NULL}, then each element is
  the result of \code{FUN}.
}
\description{
  Extract MCMC samples as a list of chains, Each chain is a
  named list of parameter arrays.
}
\examples{
data(line_samples)
line_samples_chain_pars <- mcmcdb_samples_chain_parameters(line_samples)
lapply(line_samples_chain_pars[[1]], dim)
}
\seealso{
  Other Mcmcdb methods: \code{\link{Mcmcdb-class}},
  \code{\link{McmcdbMem-class}},
  \code{\link{McmcdbWide-class}},
  \code{\link{mcmcdb_flatpars}},
  \code{\link{mcmcdb_flatpars}},
  \code{\link{mcmcdb_flatpars-methods}},
  \code{\link{mcmcdb_pardims}},
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
  \code{\link{mcmcdb_samples_parameters_chain}},
  \code{\link{mcmcdb_samples_parameters_chain}},
  \code{\link{summary}}, \code{\link{summary-methods}}
}

