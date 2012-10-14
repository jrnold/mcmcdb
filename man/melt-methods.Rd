\name{melt-methods}
\alias{melt-methods}
\alias{melt,mcmc-method}
\alias{melt,mcmc.list-method}
\title{Methods for function \code{melt}}
\arguments{
  \item{data}{Object containing the MCMC samples.}

  \item{chainid}{Set chain number; for \code{mcmc}
  objects.}
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
\section{Methods}{
  \describe{ \item{\code{signature(data="mcmc")}}{Method
  for \code{mcmc}}
  \item{\code{signature(data="mcmc.list")}}{Method for
  \code{mcmc.list}} }
}
\seealso{
  \code{\link[reshape2]{melt}} for the generic function.
}

