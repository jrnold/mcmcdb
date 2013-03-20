\name{mcmcdb_unflatten-method}
\alias{mcmcdb_unflatten}
\alias{mcmcdb_unflatten,McmcdbWide,character-method}
\alias{mcmcdb_unflatten,McmcdbWide,missing-method}
\alias{mcmcdb_unflatten,matrix,McmcdbParameters-method}
\alias{mcmcdb_unflatten,matrix,function-method}
\alias{mcmcdb_unflatten,matrix,missing-method}
\alias{mcmcdb_unflatten,numeric,McmcdbParameters-method}
\alias{mcmcdb_unflatten,numeric,function-method}
\alias{mcmcdb_unflatten,numeric,missing-method}
\alias{mcmcdb_unflatten-method}
\title{Unflatten MCMC parameters}
\arguments{
  \item{x}{Flattened parameter values}

  \item{parameters}{Object mapping flattened parameters to
  parameter arrays}
}
\value{
  All methods return a named \code{list} of parameter
  arrays.
}
\description{
  Convert parameter values from their flattened form to
  their original array shapes.
}
\section{Methods}{
  \describe{ \item{\code{signature(x = "matrix", parameters
  = "function")}}{ Each row if the matrix represents a
  sample interation. The returned array for a parameter
  array with dimension \code{d} is \code{c(d, n)}, where
  \code{n} is the number of iterations.  The function
  \code{parameters} should return an object of class
  \code{McmcdbFlatpars}. }

  \item{\code{signature(x = "matrix", parameters =
  "McmcdbParameters")}}{ See method for \code{signature(x =
  "matrix", parameters = "McmcdbParameters")}. }

  \item{\code{signature(x = "matrix", parameters =
  "missing")}}{ If \code{parameters} is missing, then the
  default for \code{\link{McmcdbParameters}} is used. }

  \item{\code{signature(x = "McmcdbWide", parameters =
  "character")}}{ The character vector \code{parameters}
  specifies a subset of parameter arrays to return.}

  \item{\code{signature(x = "McmcdbWide", parameters =
  "missing")}}{ Uses the result of
  \code{mcmcdb_parameters(x)} as the paramter object. }

  \item{\code{signature(x = "numeric", parameters =
  "function")}}{ The numeric vector represents a single
  iteration. }

  \item{\code{signature(x = "numeric", parameters =
  "McmcdbParameters")}}{ See method for \code{signature(x =
  "numeric", parameters = "function")}. }

  \item{\code{signature(x = "numeric", parameters =
  "missing")}}{ See method for \code{signature(x =
  "numeric", parameters = "function")}. } }
}
\examples{
parnames <- c("alpha", "beta.1.1", "beta.1.2", "beta.2.1", "beta.2.2")
parameters <- McmcdbParameters(parnames)
samples <- rnorm(length(parnames))
names(samples) <- parnames
samples
mcmcdb_unflatten(samples)
}
\seealso{
  Other McmdbWide methods: \code{\link{mcmcdb_iply}},
  \code{\link{mcmcdb_iply-methods}}

  Other McmdbWide methods: \code{\link{mcmcdb_iply}},
  \code{\link{mcmcdb_iply-methods}}
}
\keyword{methods}

