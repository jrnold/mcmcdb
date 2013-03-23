\docType{methods}
\name{mcmdb_mcpar-methods}
\alias{mcmcdb_mcpar}
\alias{mcmcdb_mcpar,Mcmcdb-method}
\alias{mcmdb_mcpar-methods}
\title{Methods for function \code{mcmcdb_par}}
\arguments{
  \item{object}{Object}
}
\value{
  \code{data.frame} with columns \describe{
  \item{\code{chain_id}}{Chain id}
  \item{\code{n_iter}}{\code{integer}. Number of
  iterations} \item{\code{iter_start}}{\code{integer}.
  First iteration saved. Can be missing}
  \item{\code{iter_end}}{\code{integer}. Last iteration
  saved. Can be missing}
  \item{\code{iter_thin}}{\code{integer}. Thinning saved.
  Can be missing} }
}
\description{
  Get basic information about each chain: number of
  iterations, first iteration, last iteration, thinning.
}
\section{Methods}{
  \describe{ \item{\code{signature(object = "Mcmcdb")}}{} }
}
\examples{
data(line_samples)
mcmcdb_mcpar(line_samples)
}

