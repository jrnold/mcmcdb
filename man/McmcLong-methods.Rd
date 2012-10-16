\docType{methods}
\name{McmcLong-methods}
\alias{McmcLong}
\alias{McmcLong,data.frame-method}
\alias{McmcLong-methods}
\title{Create \code{McmcLong} objects}
\arguments{
  \item{data}{Object with MCMC samples}

  \item{parnames}{\code{character} vector of parameter
  names which will be parsed by \code{fun}}

  \item{fun}{\code{function} used to parse \code{parnames}.
  See \code{\link{mcmc_parse_parname_default}} for what
  this function has to return.}
}
\description{
  Create \code{McmcLong} objects
}
\section{Methods}{
  \describe{
  \item{\code{signature(data="data.frame")}}{\code{data}
  should have columns \code{c("parname", "chain_id",
  "iter", "val")}.} }
}
\keyword{methods}

