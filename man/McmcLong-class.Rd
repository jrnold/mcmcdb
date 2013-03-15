\docType{class}
\name{McmcLong-class}
\alias{McmcLong-class}
\title{MCMC Samples in long-format}
\description{
  Mcmc samples stored as a table with columns: "parname",
  "chain_id", "iter", "val". Right now the backend is a
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
  \describe{ \item{\code{samples}}{\code{McmcSamples}.}
  \item{\code{parameters}}{\code{McmcParameters}.}
  \item{\code{chains}}{\code{McmcChains}.}
  \item{\code{par_chains}}{\code{McmcParChains}.}
  \item{\code{chain_iters}}{\code{McmcChainIters}.}
  \item{\code{metadata}}{\code{list} with general data
  about the samples.}
  \item{\code{version}}{\code{character} version of
  \pkg{mcmcdb} with which the object was created} }
}
\keyword{classes}

