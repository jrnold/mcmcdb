\name{mcmcdb_samples_pararrays}
\alias{mcmcdb_samples_pararrays}
\alias{mcmcdb_samples_pararrays,McmcdbWide-method}
\title{Mcmcdb object samples (by Parameter array)}
\value{
  \code{list} with length equal to the number of parameter
  arrays. Each element of the list is an \code{array}.
}
\description{
  Return samples from a Mcmcdb object in a list, one
  element for each parameter array.
}
\examples{
data(line_samples)
summary(mcmcdb_samples_pararrays(line_samples))
}
\seealso{
  Other McmcdbWide methods: \code{\link{$}},
  \code{\link{McmcdbWide-class}}, \code{\link{[}},
  \code{\link{[[}}, \code{\link{extract-methods}},
  \code{\link{mcmcdb_chains}},
  \code{\link{mcmcdb_samples_flatpars}},
  \code{\link{mcmcdb_samples_flatpars}},
  \code{\link{mcmcdb_samples_iter}},
  \code{\link{mcmcdb_samples_iter}},
  \code{\link{mcmcdb_samples_long}},
  \code{\link{mcmcdb_samples_long}}
}

