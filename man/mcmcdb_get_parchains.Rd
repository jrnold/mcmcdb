\name{mcmcdb_get_parchains}
\alias{mcmcdb_get_parchains}
\alias{mcmcdb_get_parchains,McmcdbWide-method}
\title{Get parameter-chain data from an Mcmcdb object}
\arguments{
  \item{object}{object}

  \item{drop}{\code{logical}. If \code{TRUE}, only return
  \code{flatpar} and \code{} columns. If \code{FALSE}, then
  return all data related to the iterations (other than
  their values).}
}
\value{
  \linkS4class{McmcIters}
}
\description{
  Get parameter-chain data from an Mcmcdb object
}
\seealso{
  \code{\linkS4class{McmcdbWide}}

  Other get.methods: \code{\link{mcmcdb_get_chains}},
  \code{\link{mcmcdb_get_iters}},
  \code{\link{mcmcdb_get_parameters}}
}

