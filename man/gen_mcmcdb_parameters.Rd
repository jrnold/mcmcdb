\name{gen_mcmcdb_parameters}
\alias{gen_mcmcdb_parameters}
\title{Generate \code{McmcdbParameters} object given parameter dims}
\usage{
  gen_mcmcdb_parameters(x, style = mcmc_parnames_stan)
}
\arguments{
  \item{x}{Named \code{list} of parameter array
  dimensions.}

  \item{style}{\code{function} taking two arguments,
  parameter array name, and parameter array dimension, and
  returning a character vector of flat parameter arrays.
  E.g. \code{\link{mcmc_parnames_stan}} and
  \code{\link{mcmc_parnames_bugs}}.}
}
\description{
  Given a list of parameter array names and dimensions,
  generate a \code{\linkS4class{McmcdbParameters}}
}
\examples{
mcmcpars <- list(alpha = 1L, beta = 2L, gamma = c(2L, 2L))
gen_mcmcdb_parameters(mcmcpars)
gen_mcmcdb_parameters(mcmcpars, style=mcmc_parnames_bugs)
}

