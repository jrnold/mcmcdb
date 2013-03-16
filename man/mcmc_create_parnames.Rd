\name{mcmc_parnames_stan}
\alias{mcmc_create_parnames_bugs}
\alias{mcmc_create_parnames_stan}
\alias{mcmc_parnames_bugs}
\alias{mcmc_parnames_stan}
\title{Create BUGS/Stan parameter names}
\usage{
  mcmc_parnames_stan(x, dim)

  mcmc_parnames_bugs(x, dim)
}
\arguments{
  \item{x}{\code{character} Parameter name.}

  \item{dim}{\code{integer} Dimension of the array.}
}
\value{
  \code{character} vector of flat parameter names.
}
\description{
  Given a parameter name and a matrix of index values,
  generate names for the unlisted parameters.
}
\details{
  \describe{ \item{\code{mcmc_parnames_bugs}}{Writes
  BUGS/JAGS style flat parameter names, e.g.
  \code{"alpha[1,2]"}}.
  \item{\code{mcmc_parnames_stan}}{Writes Stan style flat
  parameter names, e.g. \code{"alpha.1.2"}}. }
}
\examples{
mcmc_parnames_bugs("alpha", c(1, 2))
mcmc_parnames_stan("alpha", c(1, 2))
}

