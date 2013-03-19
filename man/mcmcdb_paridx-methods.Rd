\name{mcmcdb_paridx-methods}
\alias{mcmcdb_paridx}
\alias{mcmcdb_paridx,McmcdbParameters-method}
\alias{mcmcdb_paridx,McmcdbWide-method}
\alias{mcmcdb_paridx-methods}
\title{Get parameter indices}
\arguments{
  \item{object}{Object}
}
\value{
  \code{list} of integer matrices. Each element of the list
  is a parameter array. Each matrix has a number of rows
  equal to the total number of flat parameters, and a
  number of columns equal to the number of dimensions of
  the parameter array. The rownames of each matrix are the
  flat parameter names.
}
\description{
  Get parameter indices
}
\seealso{
  Other McmcdbParameters methods:
  \code{\link{McmcdbParameters-class}},
  \code{\link{mcmcdb_flatpars}},
  \code{\link{mcmcdb_flatpars}},
  \code{\link{mcmcdb_flatpars-methods}},
  \code{\link{mcmcdb_pardims}},
  \code{\link{mcmcdb_pardims}},
  \code{\link{mcmcdb_pardims-methods}}

  Other McmcdbParameters methods:
  \code{\link{McmcdbParameters-class}},
  \code{\link{mcmcdb_flatpars}},
  \code{\link{mcmcdb_flatpars}},
  \code{\link{mcmcdb_flatpars-methods}},
  \code{\link{mcmcdb_pardims}},
  \code{\link{mcmcdb_pardims}},
  \code{\link{mcmcdb_pardims-methods}}
}

