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
  the samples.} \item{\code{model_data}}{\code{listOrNULL}.
  A \code{list} with the data used in the model. If the
  data is not included, then \code{NULL}.}
  \item{\code{parinit}}{\code{numericOrNULL}. A named
  \code{numeric} vector of the parameter starting values in
  the model.} \item{\code{version}}{\code{character}
  version of \pkg{mcmcdb} with which the object was
  created} }
}
\examples{
showClass("McmcdbWide")

# Example included in the package
data("line_mcmcdbwide")
print(line_mcmcdbwide)

# access data
mcmcdb_chains(line_mcmcdbwide)
mcmcdb_chains(line_mcmcdbwide, drop=TRUE)

mcmcdb_parameters(line_mcmcdbwide)
mcmcdb_pardims(line_mcmcdbwide)
mcmcdb_pararrays(line_mcmcdbwide)
mcmcdb_flatpars(line_mcmcdbwide)
mcmcdb_par_indices(line_mcmcdbwide)

mcmcdb_iters(line_mcmcdbwide)
mcmcdb_flatpar_chains(line_mcmcdbwide)
mcmcdb_metadata(line_mcmcdbwide)
}
\seealso{
  \code{\link{McmcdbWide}} for the method usually used to
  create these objects.
}

