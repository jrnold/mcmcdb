\name{rbind2}
\alias{rbind2}
\title{Mcmc.list into a matrix.}
\value{
  \code{matrix} object with columns equal to the number of
  parameters in the \code{mcmc} objects, and a number of
  rows equal to the sum of the iterations over all chains.
}
\description{
  Binds all \code{mcmc} objects in a \code{mcmc.list} into
  a single matrix. This returns a \code{matrix} instead of
  \code{mcmc} since the start/end/thin values would no
  longer make sense. However, \code{as(from, "mcmc")} will
  transfrom an \code{mcmc.list} object into a \code{mcmc}
  object with all the chains stacked on top of each other.
}

