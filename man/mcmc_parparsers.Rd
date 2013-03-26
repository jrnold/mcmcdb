\name{mcmc_parparser_scalar}
\alias{mcmc_parparser_bugs}
\alias{mcmc_parparser_guess}
\alias{mcmc_parparser_pattern}
\alias{mcmc_parparser_scalar}
\alias{mcmc_parparser_stan}
\alias{mcmc_parparser_underscore}
\title{Parse MCMC parameter names}
\usage{
  mcmc_parparser_scalar(x)

  mcmc_parparser_pattern(x, pre, sep, post)

  mcmc_parparser_stan(x)

  mcmc_parparser_bugs(x)

  mcmc_parparser_underscore(x)

  mcmc_parparser_guess(x)
}
\arguments{
  \item{x}{\code{character} vector with flat parameter
  names.}

  \item{pre}{\code{character} Pattern between parameter
  name and indices. If a pattern grouping must be used, use
  "(?: )".}

  \item{sep}{\code{character} Pattern seperating each
  index.}

  \item{post}{\code{character} Pattern following the
  indices.}
}
\value{
  Object of class \code{McmcdbFlatpars}
}
\description{
  Functions that parse a vector of flat parameter names and
  return an object of class
  \code{\linkS4class{McmcdbFlatpars}}.

  \describe{ \item{\code{mcmc_parparser_stan}}{Parses
  parameter names treating each parameter as a scalar. E.g.
  \code{"beta.1"} and \code{"beta.2"} will be treated two
  parameter arrays of size 1.}
  \item{\code{mcmc_parparser_stan}}{Parses parameter names
  in the Stan style, e.g. \code{"beta.1.1"}}
  \item{\code{mcmc_parparser_guess}}{Tries to guess the
  format of the parameters}
  \item{\code{mcmc_parparser_pattern}}{Parses parameter
  names using arbitrary patterns.} }
}
\examples{
mcmc_parparser_bugs(c("beta[1,1]", "beta[1,2]"))
mcmc_parparser_stan(c("beta.1.1", "beta.1.2"))
mcmc_parparser_underscore(c("beta_1_1", "beta_1_2"))
mcmc_parparser_pattern(c("beta<1;1>", "beta<1;2>"), "<", ";", ">")
mcmc_parparser_guess(c("beta[1,1]", "beta[1,2]"))
mcmc_parparser_guess(c("beta.1.1", "beta.1.2"))
mcmc_parparser_scalar(c("beta[1,1]", "beta[1,2]"))
# for pattern groups, you must use (?:
mcmc_parparser_pattern(c("beta<1;1>", "beta.1,2"), "[<.]", "[;,]", "(?:>|)")
}

