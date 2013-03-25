\docType{methods}
\name{mcmcdb_samples_flatpars_chain}
\alias{mcmcdb_samples_flatpars_chain}
\alias{mcmcdb_samples_flatpars_chain,Mcmcdb-method}
\title{Extract MCMC samples (Flat parameters, chains)}
\arguments{
  \item{object}{An object containing the MCMC samples.}

  \item{flatpars}{\code{character}. Flat parameters to
  include. If \code{NULL}, all flat parameters.}

  \item{pararrays}{\code{character}. Parameter arrays to
  include. If \code{NULL}, all parameter arrays. The union
  of flat parameters in \code{pararrays} and
  \code{flatpars} is included.}

  \item{chain_id}{\code{integer}. Chains to include. If
  \code{NULL}, all chains.}

  \item{iter}{\code{integer}. Iterations to include. If
  \code{NULL}, all iterations.}

  \item{...}{Options passed to internal functions.}
}
\value{
  \code{list}. Each element of the list is a \code{list} of
  \code{numeric} vectors, representing the samples of each
  flat parameter in each chain.
}
\description{
  Return MCMC samples as a list of flat parameters. Each
  flat parameter is a list of numeric vectors, by chain.
}
\examples{
data(line_samples)
line_samples_fc <- mcmcdb_samples_flatpars_chain(line_samples)
str(line_samples_fc)
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
  \code{\link{mcmcdb_samples_iter}},
  \code{\link{mcmcdb_samples_iter}},
  \code{\link{mcmcdb_samples_long}},
  \code{\link{mcmcdb_samples_long}},
  \code{\link{mcmcdb_samples_pararrays}},
  \code{\link{mcmcdb_samples_pararrays}},
  \code{\link{mcmcdb_samples_pararrays_chain}},
  \code{\link{mcmcdb_samples_pararrays_chain}}
}

