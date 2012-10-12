\docType{class}
\name{McmcLong-class}
\alias{McmcLong-class}
\title{MCMC Samples in long-format}
\description{
  Mcmc samples stored as a table with columns: "parname",
  "chainid", "iter", "val". Right now the backend is a
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

  The column names are slightly non-intuitive, but were
  chosen to avoid clashes with
  \href{http://www.postgresql.org/docs/9.2/static/sql-keywords-appendix.html}{SQL
  reserved keywords}, in order to facilitate better
  integration with SQL db backends.
}
\section{Slots}{
  \describe{ \item{\code{samples}}{\code{data.frame} with
  columns "parname", "chainid", "iter", "val"}
  \item{\code{parameters}}{\code{McmcParaterMeta} object
  with the array sizes of the paramters in the sample.}
  \item{\code{chains}}{\code{data.frame} with columns
  "chainid", "niter", "start", "end", and "thin" and other
  data for each chain.}
  \item{\code{par_chains}}{\code{data.frame} with columns
  "parname", "chainid" and other data for each parameter
  for each chain, e.g. step size multipliers for NUTS.}
  \item{\code{chain_iters}}{\code{data.frame} with columns
  "chainid", "iter" and other data for each iteration of
  each chain (which are not parameters), e.g. treedepth,
  stepsize in NUTS.} \item{\code{metadata}}{\code{list}
  with general data about the samples.} }
}
\keyword{classes}

