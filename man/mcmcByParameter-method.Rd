\docType{methods}
\name{mcmcByParameter-method}
\alias{mcmcByParameter-method}
\alias{mcmcByParameter,McmcLong-method}
\title{Apply function to the MCMC samples of each parameter}
\arguments{
  \item{object}{MCMC object}

  \item{.fun}{Function to apply to the vector of parameter
  values from all iterations.}
}
\value{
  \code{list} of length equal to the number of parameter.
  Each element contains the results of \code{.fun} for that
  parameter.
}
\description{
  Apply function to the MCMC samples of each parameter
}
\keyword{methods}

