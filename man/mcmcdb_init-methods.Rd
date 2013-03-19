\name{mcmcdb_init-methods}
\alias{mcmcdb_init}
\alias{mcmcdb_init,McmcdbWide-method}
\alias{mcmcdb_init-methods}
\title{Get parameter initial values from a Mcmcdb object}
\arguments{
  \item{object}{An object for which the method is defined.}
}
\value{
  Named \code{numeric} vector. Names are the flat parameter
  names, values are the parmeter initial values.
}
\description{
  Get the parameter initial values used in model
  estimation, if it was saved in the object.
}

