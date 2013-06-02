\docType{methods}
\name{[-methods}
\alias{[}
\alias{[,Mcmcdb,ANY,ANY-method}
\alias{[-methods}
\title{Methods for function \code{[}}
\arguments{
  \item{x}{An object.}

  \item{i}{\code{character} Flat parameter names.}

  \item{j}{\code{integer}. Chain ids.}

  \item{k}{\code{integer}. Iteration numbers.}

  \item{drop}{\code{logical}. Determines the type of object
  returned.}
}
\value{
  If \code{drop=TRUE}, a matrix is returned, otherwise a
  \code{data.frame} is returned.
}
\description{
  Methods for function \code{[}
}
\seealso{
  See \code{\linkS4class{McmcdbWide}} for examples.

  Other McmcdbWide methods: \code{\link{$}},
  \code{\link{$-methods}}, \code{\link{McmcdbWide-class}},
  \code{\link{[[}}, \code{\link{[[-methods}},
  \code{\link{mcmcdb_chains}},
  \code{\link{mcmcdb_drop_chain}},
  \code{\link{mcmcdb_drop_chain}},
  \code{\link{mcmcdb_drop_chain}},
  \code{\link{mcmcdb_drop_iters}},
  \code{\link{mcmcdb_drop_iters}},
  \code{\link{mcmcdb_drop_iters}},
  \code{\link{mcmcdb_drop_parameter}},
  \code{\link{mcmcdb_drop_parameter}},
  \code{\link{mcmcdb_samples_flatpars}},
  \code{\link{mcmcdb_samples_flatpars}},
  \code{\link{mcmcdb_samples_iter}},
  \code{\link{mcmcdb_samples_iter}},
  \code{\link{mcmcdb_samples_long}},
  \code{\link{mcmcdb_samples_long}},
  \code{\link{mcmcdb_samples_parameters}},
  \code{\link{mcmcdb_samples_parameters}},
  \code{\link{mcmcdb_unflatten}},
  \code{\link{mcmcdb_unflatten}},
  \code{\link{mcmcdb_unflatten}},
  \code{\link{mcmcdb_unflatten}},
  \code{\link{mcmcdb_unflatten}},
  \code{\link{mcmcdb_unflatten}},
  \code{\link{mcmcdb_unflatten}},
  \code{\link{mcmcdb_unflatten}},
  \code{\link{mcmcdb_unflatten-method}}
}

