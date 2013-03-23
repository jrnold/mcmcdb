\name{mcmcdb_flatpar_chains}
\alias{mcmcdb_flatpar_chains}
\alias{mcmcdb_flatpar_chains,McmcdbMem-method}
\title{Get flat parameter, chains from an Mcmcdb object}
\arguments{
  \item{object}{object}

  \item{drop}{\code{logical}. If \code{TRUE}, only return
  \code{flatpar} and \code{} columns. If \code{FALSE}, then
  return all data related to the iterations (other than
  their values).}
}
\value{
  An object of \code{\linkS4class{McmcdbFlatparChains}}
  with flat parameters and chains.
}
\description{
  Get flat parameter, chains from an Mcmcdb object
}
\seealso{
  Other get-methods: \code{\link{mcmcdb_chains}},
  \code{\link{mcmcdb_iters}}
}

