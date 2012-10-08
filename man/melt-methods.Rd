\name{melt-methods}
\alias{melt-methods}
\alias{melt,mcmc-method}
\alias{melt,mcmc.list-method}
\title{Methods for function \code{melt}}
\arguments{
  \item{data}{Object containing the MCMC samples.}

  \item{chain}{Set chain number; for \code{mcmc} objects.}
}
\value{
  \code{data.frame} with columns \describe{
  \item{\code{chain}}{\code{integer}}
  \item{\code{iteration}}{\code{integer}}
  \item{\code{parameter}}{\code{factor}}
  \item{\code{value}}{\code{numeric}} }
}
\description{
  Melt MCMC samples objects into a \code{data.frame} with
  columns for the chain, iteration, parameter, and value.
}
\details{
  \S4method{melt}{mcmc}{data, ...}

  \S4method{melt}{mcmc.list}{data, ...}
}
\seealso{
  \code{\link[reshape2]{melt}} for the generic function.

  Other res: \code{\link{mcmc-class}},
  \code{\link{mcmc.list-class}}
}

