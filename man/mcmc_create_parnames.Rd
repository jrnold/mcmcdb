\name{mcmc_create_parnames_stan}
\alias{mcmc_create_parnames_bugs}
\alias{mcmc_create_parnames_stan}
\title{Create Unlisted Parameter Names}
\usage{
  mcmc_create_parnames_stan(x, idx)

  mcmc_create_parnames_bugs(x, idx)
}
\arguments{
  \item{x}{\code{character}. Parameter name.}

  \item{idx}{\code{matrix} of indices.}
}
\description{
  Given a parameter name and a matrix of index values,
  generate names for the unlisted parameters.
}

