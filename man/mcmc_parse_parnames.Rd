\name{mcmc_parse_parnames}
\alias{mcmc_parse_parnames}
\title{Create McmcParameter object from MCMC parameter names}
\usage{
  mcmc_parse_parnames(x, parser = mcmc_parparser_bugs)
}
\arguments{
  \item{x}{\code{character} vector of parameter names}

  \item{parser}{\code{function} parse \code{x} into
  \code{McmcFlatparList}.}
}
\value{
  Object of class \code{McmcParameters}
}
\description{
  Create McmcParameter object from MCMC parameter names
}

