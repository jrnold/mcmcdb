\name{mcmcdb_get_chains}
\alias{mcmcdb_get_chains}
\alias{mcmcdb_get_chains,McmcdbWide-method}
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
  Other get.methods: \code{\link{mcmcdb_get_iters}},
  \code{\link{mcmcdb_get_parameters}},
  \code{\link{mcmcdb_get_parchains}}
}

