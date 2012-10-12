\docType{class}
\name{McmcLong-class}
\alias{McmcLong-class}
\title{MCMC Samples in long-format}
\description{
  Mcmc samples stored as a table with columns: "parameter",
  "chain", "iteration", "value". Right now the backend is a
  \code{data.frame}, so it is pretty damn slow, but this
  can be changed in the future. However, it seems the most
  natural for storage in a database, with the ability to
  aggregate rows, columns, etc. I'll probably write a
  version that uses a SQLite backend.
}
\details{
  While these objects enforce multiple chains, it doesn not
  require that the chains be numbered \code{1:n}. This
  means it is harder to iterate over chains, but easier to
  create distinct chain objects to combine later.
}
\section{Slots}{
  \describe{ \item{\code{samples}}{\code{data.frame} with
  columns "paramter", "chain", "iteration", "value"}
  \item{\code{parameters}}{\code{McmcParaterMeta} object
  with the array sizes of the paramters in the sample.}
  \item{\code{chainids}}{\code{data.frame} with columns
  "chainid" and other data for each chain.}
  \item{\code{par_chainids}}{\code{data.frame} with columns
  "parname", "chainid" and other data for each parameter
  for each chain, e.g. step size multipliers for NUTS.}
  \item{\code{chain_iters}}{\code{data.frame} with columns
  "chainid", "iter" and other data for each iteration of
  each chain (which are not parameters), e.g. treedepth,
  stepsize in NUTS.} \item{\code{metadata}}{\code{list}
  with general data about the samples.} }
}
\keyword{classes}

