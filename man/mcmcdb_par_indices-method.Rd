\name{mcmcdb_par_indices-method}
\alias{mcmcdb_par_indices}
\alias{mcmcdb_par_indices,McmcParameters-method}
\alias{mcmcdb_par_indices-method}
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

