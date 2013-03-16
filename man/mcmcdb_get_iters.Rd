\name{mcmcdb_get_iters}
\alias{mcmcdb_get_iters}
\alias{mcmcdb_get_iters,McmcdbWide-method}
\title{Get iteration data from Mcmcdb object}
\arguments{
  \item{object}{object}

  \item{drop}{\code{logical}. If \code{TRUE}, only return
  \code{chain_id} and \code{iter} columns. If \code{FALSE},
  then return all data related to the iterations (other
  than their values).}
}
\value{
  \linkS4class{McmcIters}
}
\description{
  Get iteration data from Mcmcdb object
}
\seealso{
  \code{\linkS4class{McmcdbWide}}

  Other get.methods: \code{\link{mcmcdb_get_chains}},
  \code{\link{mcmcdb_get_parameters}},
  \code{\link{mcmcdb_get_parchains}}
}

