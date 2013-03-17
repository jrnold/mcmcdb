\name{mcmc_parse_parnames}
\alias{mcmc_parse_parnames}
\title{Create McmcdbParameter object from MCMC parameter names}
\usage{
  mcmc_parse_parnames(x, parser = mcmc_parparser_bugs)
}
\arguments{
  \item{x}{\code{character} vector of parameter names}

  \item{parser}{\code{function} parse \code{x} into
  \linkS4class{McmcdbFlatparList}.}
}
\value{
  Object of class \linkS4class{McmcdbParameters}
}
\description{
  Create McmcdbParameter object from MCMC parameter names
}
\examples{
mcmc_parse_parnames(c("beta[1]", "beta[2]"))
mcmc_parse_parnames(c("beta.1", "beta.2"), mcmc_parparser_stan)
}

