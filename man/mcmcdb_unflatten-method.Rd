\name{mcmcdb_unflatten-method}
\alias{mcmcdb_unflatten}
\alias{mcmcdb_unflatten,matrix,McmcdbParameters-method}
\alias{mcmcdb_unflatten,numeric,McmcdbParameters-method}
\alias{mcmcdb_unflatten-method}
\title{Unflatten MCMC parameters}
\arguments{
  \item{x}{Flattened parameter values}

  \item{parameters}{Object mapping flattened parameters to
  parameter arrays}

  \item{pararrays}{\code{character} or \code{NULL}. Names
  of parameter arrays to use. If \code{NULL}, then it will
  use all parameter arrays that are in \code{parameters}
  and \code{x}. Any flat parameters implied by the
  parameter arrays in \code{paramters} which do not appear
  in the columns of \code{x} will be set to 0 in the
  returned arrays.}
}
\value{
  Named \code{list} of parameter arrays
}
\description{
  Unflatten MCMC parameters
}

