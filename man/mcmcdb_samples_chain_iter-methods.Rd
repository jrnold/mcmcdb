\docType{methods}
\name{mcmcdb_samples_chain_iter}
\alias{mcmcdb_samples_chain_iter}
\alias{mcmcdb_samples_chain_iter,Mcmcdb-method}
\title{Extract MCMC samples (Chains, Iterations)}
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
  Other McmcdbWide methods: \code{\link{$}},
  \code{\link{$-methods}}, \code{\link{McmcdbWide-class}},
  \code{\link{[}}, \code{\link{[-methods}},
  \code{\link{[[}}, \code{\link{[[-methods}},
  \code{\link{mcmcdb_chains}},
  \code{\link{mcmcdb_samples_chain_flatpars}},
  \code{\link{mcmcdb_samples_chain_flatpars}},
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
  \code{\link{mcmcdb_samples_pararrays}},
  \code{\link{mcmcdb_samples_pararrays_chain}},
  \code{\link{mcmcdb_samples_pararrays_chain}}
}

