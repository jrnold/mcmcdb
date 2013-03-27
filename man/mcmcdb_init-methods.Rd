\name{mcmcdb_init-methods}
\alias{mcmcdb_init}
\alias{mcmcdb_init,McmcdbMem-method}
\alias{mcmcdb_init-methods}
\title{Get parameter initial values from a Mcmcdb object}
\arguments{
  \item{object}{An object for which the method is defined.}

  \item{flatten}{\code{logical} Whether to return the
  parameter values in their flattened or original array
  shapes.}
}
\value{
  If \code{flattened=TRUE}, then a \code{matrix}; rows are
  chains, columns are flat parameters. If
  \code{flattened=FALSE}, then a list of numeric
  \code{array} objects; the elements of the list are
  chains, the arrays are the parameter arrays.
}
\description{
  Get the parameter initial values used in model
  estimation, if it was saved in the object.
}

