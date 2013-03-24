\docType{methods}
\name{extract-methods}
\alias{$}
\alias{$,McmcdbWide-method}
\alias{[}
\alias{[,McmcdbWide,ANY,ANY-method}
\alias{[[}
\alias{[[,McmcdbWide-method}
\alias{extract-methods}
\title{Extract methods}
\arguments{
  \item{i}{\code{character} Parameter name. For \code{[},
  the flattened parameter name. For \code{[[} and \code{$},
  the parameter array name.}

  \item{j}{\code{integer}. Chain number.}

  \item{k}{\code{integer}. Iteration number.}

  \item{drop}{\code{logical}. Determines the object
  returned. In general, if}
}
\value{
  If \code{drop=TRUE}, a matrix is returned, otherwise a
  \code{data.frame} is returned.
}
\description{
  The methods \code{[}, \code{[[} and \code{$} are defined
  for \code{\linkS4class{McmcdbWide}} objects to be able to
  select sample values by parameter (both flat parameters
  and parameter arrays), chain, and iteration.
}
\seealso{
  Other McmcdbWide methods: \code{\link{McmcdbWide-class}},
  \code{\link{mcmcdb_chains}},
  \code{\link{mcmcdb_samples_flatpars}},
  \code{\link{mcmcdb_samples_flatpars}},
  \code{\link{mcmcdb_samples_iter}},
  \code{\link{mcmcdb_samples_iter}},
  \code{\link{mcmcdb_samples_long}},
  \code{\link{mcmcdb_samples_long}},
  \code{\link{mcmcdb_samples_pararrays}},
  \code{\link{mcmcdb_samples_pararrays}}
}

