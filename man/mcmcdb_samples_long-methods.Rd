\docType{methods}
\name{mcmcdb_samples_long}
\alias{mcmcdb_samples_long}
\alias{mcmcdb_samples_long,McmcdbWide-method}
\title{Extract MCMC Samples (Long form)}
\arguments{
  \item{object}{An object containing the MCMC samples.}

  \item{flatpars}{\code{character}. Flat parameters to
  include. If \code{NULL}, all flat parameters.}

  \item{pararrays}{\code{character}. Parameter arrays to
  include. If \code{NULL}, all parameter arrays. The union
  of flat parameters in \code{pararrays} and
  \code{flatpars} is included.}

  \item{chain_id}{\code{integer}. Chains to include. If
  \code{NULL}, all chains.}

  \item{iter}{\code{integer}. Iterations to include. If
  \code{NULL}, all iterations.}

  \item{...}{Options passed to internal functions.}
}
\value{
  A \code{data.frame} with columns \describe{
  \item{\code{chain_id}}{\code{integer}. Chain id}
  \item{\code{iter}}{\code{integer}. Iteration number}
  \item{\code{flatpar}}{\code{factor}. Flat parameter name}
  \item{\code{val}}{\code{numeric}. Parameter values} }
}
\description{
  Extract MCMC samples from an object as a data frame, with
  columns for the flat parameter, chain, iteration, and
  value.
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
  \code{\link{$-methods}}, \code{\link{McmcdbWide-class}},
  \code{\link{[}}, \code{\link{[-methods}},
  \code{\link{[[}}, \code{\link{[[-methods}},
  \code{\link{mcmcdb_chains}},
  \code{\link{mcmcdb_drop_chain}},
  \code{\link{mcmcdb_drop_chain}},
  \code{\link{mcmcdb_drop_chain}},
  \code{\link{mcmcdb_drop_iters}},
  \code{\link{mcmcdb_drop_iters}},
  \code{\link{mcmcdb_drop_iters}},
  \code{\link{mcmcdb_drop_pararray}},
  \code{\link{mcmcdb_drop_pararray}},
  \code{\link{mcmcdb_samples_flatpars}},
  \code{\link{mcmcdb_samples_flatpars}},
  \code{\link{mcmcdb_samples_iter}},
  \code{\link{mcmcdb_samples_iter}},
  \code{\link{mcmcdb_samples_pararrays}},
  \code{\link{mcmcdb_samples_pararrays}}
}

