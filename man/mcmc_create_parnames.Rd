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
  \item{x}{\code{character}. Parameter name.}

  \item{dim}{\code{integer} dimension of the array.}
}
\description{
  Given a parameter name and a matrix of index values,
  generate names for the unlisted parameters.

  Create MCMC parnames
}

