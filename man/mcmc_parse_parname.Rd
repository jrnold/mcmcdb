\name{mcmc_parse_parname_default}
\alias{mcmc_parse_parname_bugs}
\alias{mcmc_parse_parname_default}
\alias{mcmc_parse_parname_stan}
\title{Parse MCMC parameter names}
\usage{
  mcmc_parse_parname_default(x)

  mcmc_parse_parname_stan(x)

  mcmc_parse_parname_bugs(x)
}
\arguments{
  \item{x}{\code{character} vector of character names.}
}
\value{
  \code{McmcParnames} data.frame.
}
\description{
  These functions parse a character vector of parameter
  names and return a \code{McmcParnames} object.
}

