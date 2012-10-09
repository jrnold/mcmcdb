\docType{class}
\name{McmcLong}
\alias{McmcLong}
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
\section{Slots}{
  \describe{ \item{\code{.Data}}{\code{data.frame} with
  columns "paramter", "chain", "iteration", "value"}
  \item{\code{parameters}}{\code{McmcParaterMeta} object
  with the array sizes of the paramters in the sample.} }
}

\section{Extends}{
  \describe{ \item{\code{data.frame}}{directly} }
}
\keyword{classes}

