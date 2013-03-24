\name{McmcdbParameters}
\alias{McmcdbParameters}
\alias{McmcdbParameters,McmcdbFlatpars-method}
\alias{McmcdbParameters,character-method}
\alias{McmcdbParameters,data.frame-method}
\alias{McmcdbParameters,list-method}
\alias{McmcdbParameters,missing-method}
\alias{McmcdbParameters-methods}
\title{Create McmcbParameter objects}
\arguments{
  \item{x}{object}

  \item{parser}{A function that returns an object of
  \code{\linkS4class{McmcdbFlatpars}}. For example,
  \code{\link{mcmc_parparser_stan}} or
  \code{\link{mcmc_parparser_guess}}.}
}
\description{
  Functions to create \code{\linkS4class{McmcdbParameters}}
  objects.
}
\examples{
McmcdbParameters(c("alpha", "beta.1", "beta.2"))
McmcdbParameters(c("alpha", "beta[1]", "beta[2]"),
                parser = mcmc_parparser_bugs)
}

