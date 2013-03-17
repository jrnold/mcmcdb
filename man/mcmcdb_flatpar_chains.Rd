\name{mcmcdb_flatpar_chains}
\alias{mcmcdb_flatpar_chains}
\alias{mcmcdb_flatpar_chains,McmcdbWide-method}
\title{Get parameter-chain data from an Mcmcdb object}
\arguments{
  \item{object}{object}

  \item{drop}{\code{logical}. If \code{TRUE}, only return
  \code{flatpar} and \code{} columns. If \code{FALSE}, then
  return all data related to the iterations (other than
  their values).}
}
\value{
  \linkS4class{McmcdbIters}
}
\description{
  Get parameter-chain data from an Mcmcdb object
}
\seealso{
  \code{\linkS4class{McmcdbWide}}

  Other McmcdbWide-methods: \code{\link{mcmcdb_chains}}

  Other get-methods: \code{\link{mcmcdb_chains}},
  \code{\link{mcmcdb_iters}},
  \code{\link{mcmcdb_parameters}}
}

