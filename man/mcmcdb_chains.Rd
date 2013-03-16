\name{mcmcdb_chains}
\alias{mcmcdb_chains}
\alias{mcmcdb_chains,McmcdbWide-method}
\title{Get Chains data from Mcmcdb object}
\arguments{
  \item{object}{object}

  \item{drop}{\code{logical} Include data related to the
  chains or just}
}
\value{
  If \code{drop=TRUE} then \code{integer}, if
  \code{drop=FALSE}, then \code{\linkS4class{McmcChains}}.
}
\description{
  Get Chains data from Mcmcdb object
}
\seealso{
  \code{\linkS4class{McmcdbWide}}

  Other get.methods: \code{\link{mcmcdb_iters}},
  \code{\link{mcmcdb_parameters}},
  \code{\link{mcmcdb_parchains}}
}

