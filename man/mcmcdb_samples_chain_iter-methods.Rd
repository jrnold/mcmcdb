\docType{methods}
\name{mcmcdb_samples_chain_iter}
\alias{mcmcdb_samples_chain_iter}
\alias{mcmcdb_samples_chain_iter,Mcmcdb-method}
\title{Extract MCMC samples (Chains, Iterations)}
\arguments{
  \item{object}{An object containing the MCMC samples.}

  \item{parameters}{\code{character}. Parameter arrays to
  include. If \code{NULL}, all parameter arrays.}

  \item{chain_id}{\code{integer}. Chains to include. If
  \code{NULL}, all chains.}

  \item{iter}{\code{integer}. Iterations to include. If
  \code{NULL}, all iterations.}

  \item{...}{Options passed to internal functions.}
}
\value{
  \code{list}. Each element represents a chain. Each
  element of the chain is a \code{list} of iterations. Each
  iteration is a list of \code{array} objects representing
  parameter arrays.
}
\description{
  Return MCMC samples as a list of chains. Each chain is a
  list of the iterations in that chain. Each iteration is a
  list of parameter arrays.
}
\examples{
data(line_samples)
line_samples_ci <- mcmcdb_samples_chain_iter(line_samples)
length(line_samples_ci)
# 1st iteration from the 1st chain
line_samples_ci[[1]][[1]]
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
  \code{\link{mcmcdb_samples_chain_parameters}},
  \code{\link{mcmcdb_samples_chain_parameters}},
  \code{\link{mcmcdb_samples_flatpars_chain}},
  \code{\link{mcmcdb_samples_flatpars_chain}},
  \code{\link{mcmcdb_samples_parameters_chain}},
  \code{\link{mcmcdb_samples_parameters_chain}},
  \code{\link{summary}}, \code{\link{summary-methods}}
}

