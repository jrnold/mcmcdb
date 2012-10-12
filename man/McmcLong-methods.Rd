\docType{methods}
\name{McmcLong-methods}
\alias{McmcLong}
\alias{McmcLong,McmcList2-method}
\alias{McmcLong,data.frame-method}
\alias{McmcLong-methods}
\title{Create \code{McmcLong} objects}
\arguments{
  \item{data}{Object with MCMC samples}

  \item{parnames}{\code{character} vector of parameter
  names which will be parsed by \code{fun}}

  \item{fun}{\code{function} used to parse \code{parnames}.
  See \code{\link{parse_parameter_names_default}} for what
  this function has to return.}
}
\description{
  Create \code{McmcLong} objects
}
\section{Methods}{
  \describe{
  \item{\code{signature(data="data.frame")}}{\code{data}
  should have columns \code{c("parname", "chainid", "iter",
  "val")}.} }
}
\keyword{methods}

