\name{mcmc_parparser_scalar}
\alias{mcmc_parparser_bugs}
\alias{mcmc_parparser_scalar}
\alias{mcmc_parparser_stan}
\title{Parse MCMC parameter names}
\usage{
  mcmc_parparser_scalar(x)

  mcmc_parparser_stan(x)

  mcmc_parparser_bugs(x)
}
\arguments{
  \item{x}{\code{character} vector with flat parameter
  names.}
}
\value{
  Object of class \code{McmcFlatparList}
}
\description{
  Parse MCMC parameter names
}

