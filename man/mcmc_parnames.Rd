\name{mcmc_parnames_pattern}
\alias{mcmc_parname_pattern}
\alias{mcmc_parnames_bugs}
\alias{mcmc_parnames_pattern}
\alias{mcmc_parnames_stan}
\alias{mcmc_parnames_underscore}
\title{Create paramter names for flattened parameters}
\usage{
  mcmc_parnames_pattern(x, d, pre = ".", sep = ".",
    post = "", colmajor = TRUE)

  mcmc_parnames_stan(x, d, colmajor = TRUE)

  mcmc_parnames_bugs(x, d, colmajor = TRUE)

  mcmc_parnames_underscore(x, d, colmajor = TRUE)
}
\arguments{
  \item{x}{\code{character} Parameter name.}

  \item{d}{\code{integer} Dimension of the array.}

  \item{pre}{\code{character} String to put before
  indices.}

  \item{sep}{\code{character} String used to seperate
  indices.}

  \item{post}{\code{character} String to put after
  indices.}

  \item{colmajor}{\code{logical}. If \code{TRUE}, then
  indices are in column-major order (R's default), else
  row-major.}
}
\value{
  \code{character} vector of flat parameter names.
}
\description{
  Given a parameter name and a matrix of index values,
  generate names for the unlisted parameters.

  \describe{ \item{\code{mcmc_parnames_bugs}}{Writes
  BUGS/JAGS style flat parameter names, e.g.
  \code{"alpha[1,2]"}}.
  \item{\code{mcmc_parnames_stan}}{Writes Stan style flat
  parameter names, e.g. \code{"alpha.1.2"}}.
  \item{\code{mcmc_parnames_underscore}}{Writes parameter
  names with indexes seperated by underscores, e.g.
  \code{"alpha_1_2"}}.
  \item{\code{mcmc_parnames_pattern}}{Writes parameter
  names with arbitrary patterns.} }
}
\examples{
mcmc_parnames_bugs("alpha", c(1, 2))
mcmc_parnames_stan("alpha", c(1, 2))
mcmc_parnames_underscore("alpha", c(1, 2))
mcmc_parnames_pattern("alpha", c(1, 2), "<", ";", ">")
}

