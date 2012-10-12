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
  \code{data.frame} with rownames equal to \code{x} and two
  columns \describe{ \item{\code{index}}{Comma seperated
  string of the index of the flat parameter in the array
  parameter. E.g. If "beta.1.2" is the first column, second
  row of "beta", then this should be equal to "1,2".}
  \item{\code{name}}{Name of the array parameter. E.g. If
  "beta.1.2" is the first column, second row of "beta",
  then this should be equal to "beta".} }
}
\description{
  These functions parse a character vector of parameter
  names and return a \code{data.frame} (described below)
  that can easily be turned in a \code{McmcParameters}
  object.
}

