\name{mcmcdb_iters}
\alias{mcmcdb_iters}
\alias{mcmcdb_iters,McmcdbMem-method}
\title{Get iteration data from Mcmcdb object}
\arguments{
  \item{object}{object}

  \item{drop}{\code{logical}. If \code{TRUE}, only return
  \code{chain_id} and \code{iter} columns. If \code{FALSE},
  then return all data related to the iterations (other
  than their values).}
}
\value{
  An object of \linkS4class{McmcdbIters} with chains and
  iterations, and associated data, if \code{drop=FALSE}.
}
\description{
  Get iteration data from Mcmcdb object
}
\seealso{
  Other McmcdbMem methods: \code{\link{mcmcdb_metadata}},
  \code{\link{mcmcdb_parameters}},
  \code{\link{mcmcdb_parameters-methods}}

  Other get-methods: \code{\link{mcmcdb_chains}},
  \code{\link{mcmcdb_flatpar_chains}}
}

