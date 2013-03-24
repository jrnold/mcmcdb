\name{mcmcdb_samples_long}
\alias{mcmcdb_samples_long}
\alias{mcmcdb_samples_long,McmcdbWide-method}
\title{Mcmcdb Object samples (Long form)}
\value{
  \code{data.frame} with columns \describe{
  \item{\code{chain_id}}{\code{integer}. Chain id}
  \item{\code{iter}}{\code{integer}. Iteration number}
  \item{\code{flatpar}}{\code{factor}. Flat parameter name}
  \item{\code{val}}{\code{numeric}. Parameter values} }
}
\description{
  Return samples from a Mcmcdb object in a matrix
  (iterations x long).
}
\examples{
library(plyr)
data(line_samples)
line_long <- mcmcdb_samples_long(line_samples)
head(line_long)
summary(line_long)
ddply(line_long, "flatpar",
      summarise, mean = mean(val), median = median(val))
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
  \code{\link{mcmcdb_samples_pararrays}},
  \code{\link{mcmcdb_samples_pararrays}}
}

