\docType{methods}
\name{mcmcdb_samples_flatpars_chain}
\alias{mcmcdb_samples_flatpars_chain}
\alias{mcmcdb_samples_flatpars_chain,Mcmcdb-method}
\title{Extract MCMC samples (Flat parameters, chains)}
\arguments{
  \item{object}{An object containing the MCMC samples.}

  \item{flatpars}{\code{character}. Flat parameters to
  include. If \code{NULL}, all flat parameters.}

  \item{parameters}{\code{character}. Parameter arrays to
  include. If \code{NULL}, all parameter arrays. The union
  of flat parameters in \code{parameters} and
  \code{flatpars} is included.}

  \item{chain_id}{\code{integer}. Chains to include. If
  \code{NULL}, all chains.}

  \item{iter}{\code{integer}. Iterations to include. If
  \code{NULL}, all iterations.}

  \item{FUN}{\code{function}. Function to apply to each
  flat paramter. Function of the form \code{function(x)},
  where \code{x} is a \code{list} of \code{numeric}
  vectors, representing the flat parameters in each chain.}

  \item{...}{Options passed to internal functions.}
}
\value{
  \code{list}. If \code{FUN = NULL}, then each element of
  the list is a \code{list} of \code{numeric} vectors,
  representing the samples of each flat parameter in each
  chain. If \code{FUN != NULL}, then the elements are the
  results of \code{FUN}.
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
  \code{\link{mcmcdb_samples_chain_parameters}},
  \code{\link{mcmcdb_samples_chain_parameters}},
  \code{\link{mcmcdb_samples_parameters_chain}},
  \code{\link{mcmcdb_samples_parameters_chain}},
  \code{\link{summary}}, \code{\link{summary-methods}}
}

