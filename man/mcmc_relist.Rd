\name{mcmc_relist}
\alias{mcmc_relist}
\title{Relist MCMC samples}
\usage{
  mcmc_relist(parnames, pararrays, flesh)
}
\arguments{
  \item{param}{parnames \code{McmcParnames} object.}

  \item{param}{pararrays \code{McmcPararrays} object.}

  \item{param}{flesh Named \code{numeric} vector with names
  corresponding to the parameters.}
}
\description{
  Convert a \code{numeric} vector with MCMC samples, into a
  list of arrays with the original dimensions of those
  parameters.
}
\seealso{
  \code{\link{relist}}
}

