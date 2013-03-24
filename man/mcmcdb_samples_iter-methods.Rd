\name{mcmcdb_samples_iter}
\alias{mcmcdb_samples_iter}
\alias{mcmcdb_samples_iter,McmcdbWide-method}
\title{Mcmcdb Object samples (by Iteration)}
\value{
  \code{list} with length equal to the number of elements.
  Each element of the list is a \code{list} of \code{array}
  objects.
}
\description{
  Return samples from a Mcmcdb object in a list, one
  element for each iteration. Each element is a list of
  arrays, one for each parameter in its original
  dimensions.
}
\examples{
data(line_samples)
line_samples_iter <- mcmcdb_samples_iter(line_samples)
length(line_samples_iter)
line_samples_iter[[1]]
}
\seealso{
  Other McmcdbWide methods: \code{\link{$}},
  \code{\link{McmcdbWide-class}}, \code{\link{[}},
  \code{\link{[[}}, \code{\link{extract-methods}},
  \code{\link{mcmcdb_chains}},
  \code{\link{mcmcdb_samples_flatpars}},
  \code{\link{mcmcdb_samples_flatpars}},
  \code{\link{mcmcdb_samples_long}},
  \code{\link{mcmcdb_samples_long}},
  \code{\link{mcmcdb_samples_pararrays}},
  \code{\link{mcmcdb_samples_pararrays}}
}

