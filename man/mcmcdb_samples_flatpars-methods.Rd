\name{mcmcdb_samples_flatpars}
\alias{mcmcdb_samples_flatpars}
\alias{mcmcdb_samples_flatpars,McmcdbWide-method}
\title{Mcmcdb Object samples (Flatpars form)}
\value{
  \code{matrix}. Columns are the flatpars. Rows are the
  iterations (from all chains).
}
\description{
  Return samples from a Mcmcdb object in a matrix
  (iterations x flatpars).
}
\examples{
data(line_samples)
line_wide <- mcmcdb_samples_flatpars(line_samples)
dim(line_wide)
head(line_wide)
summary(line_wide)
}
\seealso{
  Other McmcdbWide methods: \code{\link{$}},
  \code{\link{McmcdbWide-class}}, \code{\link{[}},
  \code{\link{[[}}, \code{\link{extract-methods}},
  \code{\link{mcmcdb_aply}},
  \code{\link{mcmcdb_aply-methods}},
  \code{\link{mcmcdb_chains}}, \code{\link{mcmcdb_fply}},
  \code{\link{mcmcdb_fply-methods}},
  \code{\link{mcmcdb_samples_long}},
  \code{\link{mcmcdb_samples_long}}
}

