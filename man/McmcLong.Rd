\docType{methods}
\name{McmcLong}
\alias{McmcLong}
\title{Create \code{McmcLong} objects}
\arguments{
  \item{data}{Object with MCMC samples}

  \item{parameter_name}{\code{character} vector of
  parameter names which will be parsed by \code{fun}}

  \item{fun}{\code{function} used to parse
  \code{parameter_name}. See
  \code{\link{parse_parameter_names_default}} for what this
  function has to return.}
}
\description{
  Create \code{McmcLong} objects
}
\section{Methods}{
  \describe{
  \item{\code{signature(data="data.frame")}}{\code{data}
  should have columns \code{c("parameter", "chain",
  "iteration", "value")}.} }
}
\keyword{methods}

