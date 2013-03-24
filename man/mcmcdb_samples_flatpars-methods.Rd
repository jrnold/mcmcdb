\docType{methods}
\name{mcmcdb_samples_flatpars}
\alias{mcmcdb_samples_flatpars}
\alias{mcmcdb_samples_flatpars,McmcdbWide-method}
\title{Extract MCMC Samples (Flat parameter form)}
\arguments{
  \item{object}{An object containing the MCMC samples.}

  \item{flatpars}{\code{character}. Flat parameters to
  include. If \code{NULL}, all flat parameters.}

  \item{pararrays}{\code{character}. Parameter arrays to
  include. If \code{NULL}, all parameter arrays. The union
  of flat paramters in \code{pararrays} and \code{flatpars}
  is included.}

  \item{chain_id}{\code{integer}. Chains to include. If
  \code{NULL}, all chains.}

  \item{iter}{\code{integer}. Iterations to include. If
  \code{NULL}, all iterations.}

  \item{drop}{\code{logical}. If \code{TRUE}, and only a
  single flat parameter is returned, the result is coerced
  to a \code{numeric} vector.}

  \item{...}{Options passed to internal functions.}
}
\value{
  \code{matrix}. Columns are flat parameters. Rows are
  iterations, from all chains.
}
\description{
  Extract MCMC samples from an object as a a matrix of flat
  parameters.
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
  \code{\link{$-methods}}, \code{\link{McmcdbWide-class}},
  \code{\link{[}}, \code{\link{[}}, \code{\link{[}},
  \code{\link{[-methods}}, \code{\link{[[}},
  \code{\link{[[-methods}}, \code{\link{mcmcdb_chains}},
  \code{\link{mcmcdb_samples_iter}},
  \code{\link{mcmcdb_samples_iter}},
  \code{\link{mcmcdb_samples_long}},
  \code{\link{mcmcdb_samples_long}},
  \code{\link{mcmcdb_samples_pararrays}},
  \code{\link{mcmcdb_samples_pararrays}}
}

