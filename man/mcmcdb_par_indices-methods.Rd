\name{mcmcdb_par_indices-methods}
\alias{mcmcdb_par_indices}
\alias{mcmcdb_par_indices,McmcdbParameters-method}
\alias{mcmcdb_par_indices-methods}
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
  \code{\link{mcmcdb_pararrays}},
  \code{\link{mcmcdb_pararrays}},
  \code{\link{mcmcdb_pararrays-methods}},
  \code{\link{mcmcdb_pardims}},
  \code{\link{mcmcdb_pardims}},
  \code{\link{mcmcdb_pardims-methods}}

  Other McmcdbWide methods: \code{\link{McmcdbWide-class}},
  \code{\link{mcmcdb_flatpars}},
  \code{\link{mcmcdb_flatpars}},
  \code{\link{mcmcdb_flatpars-methods}},
  \code{\link{mcmcdb_iters}},
  \code{\link{mcmcdb_metadata}},
  \code{\link{mcmcdb_parameters}},
  \code{\link{mcmcdb_pararrays}},
  \code{\link{mcmcdb_pararrays}},
  \code{\link{mcmcdb_pararrays-methods}},
  \code{\link{mcmcdb_pardims}},
  \code{\link{mcmcdb_pardims}},
  \code{\link{mcmcdb_pardims-methods}}
}

