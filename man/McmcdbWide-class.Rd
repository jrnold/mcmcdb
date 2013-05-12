\docType{class}
\name{McmcdbWide-class}
\alias{McmcdbWide-class}
\title{MCMC Samples in wide-format}
\description{
  Mcmc samples stored as a matrix with (number of chains x
  number of iterations) rows and (number of flat
  parameters) columns.
}
\section{Slots}{
  \describe{ \item{\code{samples}}{\code{matrix} containing
  the sample parameter values. This matrix has (number of
  chains * iterations) rows, and )number of flat
  parameters) columns.}
  \item{\code{parameters}}{\linkS4class{McmcdbParameters}.}
  \item{\code{chains}}{\linkS4class{McmcdbChains}.}
  \item{\code{iters}}{\linkS4class{McmcdbIters}.}
  \item{\code{flatpar_chains}}{\linkS4class{McmcdbFlatparChains}.}
  \item{\code{metadata}}{\code{list} with metadata about
  the samples.} \item{\code{model_data}}{\code{namedList}.
  The data used in the model.}
  \item{\code{version}}{\code{character} version of
  \pkg{mcmcdb} with which the object was created} }
}
\examples{
showClass("McmcdbWide")

# Example included in the package
data("line_samples")
print(line_samples)

# Extract samples
# As matrix of flat parameters
mcmcdb_samples_flatpars(line_samples)
mcmcdb_samples_pararrays(line_samples)
mcmcdb_samples_iter(line_samples)
mcmcdb_samples_long(line_samples)

# extract samples (flat parameters)
str(line_samples["beta[1]"])
line_samples["beta[1]", 1, 1:10]
summary(line_samples[["beta", 1, 1:4, drop=FALSE]])

# extract samples (parameter arrays)
str(line_samples[["beta"]])
summary(line_samples[["beta", 1, 1:4, drop=FALSE]])

# Chain information
mcmcdb_chains(line_samples)
mcmcdb_chains(line_samples, drop=FALSE)

# Iteration information
summary(mcmcdb_iters(line_samples))

# Paramater information
mcmcdb_parameters(line_samples)
mcmcdb_pardims(line_samples)
mcmcdb_flatpars(line_samples)
mcmcdb_paridx(line_samples)

# extract metadata
mcmcdb_metadata(line_samples)
}
\seealso{
  \code{\link{McmcdbWide}} for the method usually used to
  create these objects.

  Other Mcmcdb classes: \code{\link{Mcmcdb-class}},
  \code{\link{McmcdbMem-class}}

  Other Mcmcdb methods: \code{\link{Mcmcdb-class}},
  \code{\link{McmcdbMem-class}},
  \code{\link{mcmcdb_flatpars}},
  \code{\link{mcmcdb_flatpars}},
  \code{\link{mcmcdb_flatpars-methods}},
  \code{\link{mcmcdb_pardims}},
  \code{\link{mcmcdb_pardims}},
  \code{\link{mcmcdb_pardims-methods}},
  \code{\link{mcmcdb_paridx}}, \code{\link{mcmcdb_paridx}},
  \code{\link{mcmcdb_paridx-methods}},
  \code{\link{mcmcdb_samples_chain_flatpars}},
  \code{\link{mcmcdb_samples_chain_flatpars}},
  \code{\link{mcmcdb_samples_chain_iter}},
  \code{\link{mcmcdb_samples_chain_iter}},
  \code{\link{mcmcdb_samples_chain_pararrays}},
  \code{\link{mcmcdb_samples_chain_pararrays}},
  \code{\link{mcmcdb_samples_flatpars_chain}},
  \code{\link{mcmcdb_samples_flatpars_chain}},
  \code{\link{mcmcdb_samples_pararrays_chain}},
  \code{\link{mcmcdb_samples_pararrays_chain}},
  \code{\link{summary}}, \code{\link{summary-methods}}

  Other McmcdbMem methods: \code{\link{McmcdbMem-class}},
  \code{\link{mcmcdb_iters}},
  \code{\link{mcmcdb_metadata}},
  \code{\link{mcmcdb_parameters}},
  \code{\link{mcmcdb_parameters-methods}}

  Other McmcdbWide methods: \code{\link{$}},
  \code{\link{$-methods}}, \code{\link{[}},
  \code{\link{[-methods}}, \code{\link{[[}},
  \code{\link{[[-methods}}, \code{\link{mcmcdb_chains}},
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
  \code{\link{mcmcdb_samples_long}},
  \code{\link{mcmcdb_samples_long}},
  \code{\link{mcmcdb_samples_pararrays}},
  \code{\link{mcmcdb_samples_pararrays}},
  \code{\link{mcmcdb_unflatten}},
  \code{\link{mcmcdb_unflatten}},
  \code{\link{mcmcdb_unflatten}},
  \code{\link{mcmcdb_unflatten}},
  \code{\link{mcmcdb_unflatten}},
  \code{\link{mcmcdb_unflatten}},
  \code{\link{mcmcdb_unflatten}},
  \code{\link{mcmcdb_unflatten}},
  \code{\link{mcmcdb_unflatten-method}}
}

