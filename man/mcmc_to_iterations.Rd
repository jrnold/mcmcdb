\name{mcmc_to_iterations}
\alias{mcmc_to_iterations}
\title{Iterate through mcmc iterations}
\usage{
  mcmc_to_iterations(object, data = list(), FUN = identity,
    ...)
}
\arguments{
  \item{object}{\code{McmcList2} object.}

  \item{data}{\code{function} Data to combine with
  parameters on each iteration.}

  \item{FUN}{\code{function} Function to apply to each
  iteration.}

  \item{...}{Pass to \code{FUN}.}
}
\description{
  Iterate through mcmc iterations
}

