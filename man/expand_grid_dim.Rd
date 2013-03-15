\name{expand_grid_dim}
\alias{expand_grid_dim}
\title{Generate indices for all dimensions}
\usage{
  expand_grid_dim(dim)
}
\arguments{
  \item{dim}{Array dimensions}
}
\value{
  \code{matrix} with dimensions \code{c(prod(dim), dim)}.
}
\description{
  Create matrix if all indices for a given dimension
  vector.
}
\keyword{internal}

