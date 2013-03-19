\name{extract-methods}
\alias{$}
\alias{$,McmcdbWide-method}
\alias{[}
\alias{[,McmcdbWide,ANY,ANY-method}
\alias{[[}
\alias{[[,McmcdbWide-method}
\alias{extract-methods}
\title{Extract}
\arguments{
  \item{i}{\code{character} Parameter name. For \code{[},
  the flattened parameter name. For \code{[[} and \code{$},
  the parameter array name.}

  \item{j}{\code{integer}. Chain number.}

  \item{k}{\code{integer}. Iteration number.}

  \item{drop}{\code{logical}. Determines the object
  returned. In general, if \code{drop=TRUE}, then a vector
  or array is returned, otherwise a \code{data.frame} is
  returned.}
}
\value{
  TODO
}
\description{
  The methods \code{[}, \code{[[} and \code{$} are defined
  for \code{\linkS4class{McmcdbWide}} objects to be able to
  select sample values by parameter (both flat parameters
  and parameter arrays), chain, and iteration.
}

