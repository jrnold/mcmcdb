\name{mcmc_parparser_scalar}
\alias{mcmc_parparser_bugs}
\alias{mcmc_parparser_guess}
\alias{mcmc_parparser_scalar}
\alias{mcmc_parparser_stan}
\alias{mcmc_parparsers_bugs}
\alias{mcmc_parparsers_guess}
\alias{mcmc_parparsers_scalar}
\alias{mcmc_parparsers_stan}
\title{Parse MCMC parameter names}
\usage{
  mcmc_parparser_scalar(x)

  mcmc_parparser_stan(x)

  mcmc_parparser_bugs(x)

  mcmc_parparser_guess(x)
}
\arguments{
  \item{x}{\code{character} vector with flat parameter
  names.}
}
\value{
  Object of class \code{McmcdbFlatpars}
}
\description{
  Functions that parse a vector of flat parameter names and
  return an object of class
  \code{\linkS4class{McmcdbFlatpars}}. These functions are
  usually passed as arguments to
  \code{\link{McmcdbParameters}} to create
  \code{\linkS4class{McmcdbParameters}} objects.
}
\details{
  \describe{ \item{\code{mcmc_parparser_bugs}}{Parses
  parameter names in the Stan style, e.g.
  \code{"beta[1,1]"}}
  \item{\code{mcmc_parparser_stan}}{Parses parameter names
  treating each parameter as a scalar. E.g. \code{"beta.1"}
  and \code{"beta.2"} will be treated two parameter arrays
  of size 1.} \item{\code{mcmc_parparser_stan}}{Parses
  parameter names in the Stan style, e.g.
  \code{"beta.1.1"}}
  \item{\code{mcmc_parparser_guess}}{Tries to guess the
  format of the parameters} }
}
\examples{
mcmc_parparser_bugs(c("beta[1,1]", "beta[1,2]"))
mcmc_parparser_stan(c("beta.1.1", "beta.1.2"))
mcmc_parparser_guess(c("beta[1,1]", "beta[1,2]"))
mcmc_parparser_guess(c("beta.1.1", "beta.1.2"))
mcmc_parparser_scalar(c("beta[1,1]", "beta[1,2]"))
}

