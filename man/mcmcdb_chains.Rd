\name{mcmcdb_chains}
\alias{mcmcdb_chains}
\alias{mcmcdb_chains,McmcdbMem-method}
\title{Get Chains data from Mcmcdb object}
\arguments{
  \item{object}{object}

  \item{drop}{\code{logical} Include data related to the
  chains or just}
}
\value{
  If \code{drop=TRUE} then \code{integer}, if
  \code{drop=FALSE}, then
  \code{\linkS4class{McmcdbChains}}, of the chains.
}
\description{
  Get Chains data from Mcmcdb object
}
\seealso{
  Other McmcdbWide methods: \code{\link{$}},
  \code{\link{McmcdbWide-class}}, \code{\link{[}},
  \code{\link{[[}}, \code{\link{extract-methods}},
  \code{\link{mcmcdb_aply}},
  \code{\link{mcmcdb_aply-methods}},
  \code{\link{mcmcdb_fply}},
  \code{\link{mcmcdb_fply-methods}}

  Other get-methods: \code{\link{mcmcdb_flatpar_chains}},
  \code{\link{mcmcdb_iters}}
}

