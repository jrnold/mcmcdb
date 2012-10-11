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
  with the array sizes of the paramters in the sample.} }
}

\section{Extends}{
  \describe{ \item{\code{data.frame}}{directly} }
}
\keyword{classes}

