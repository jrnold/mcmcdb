\docType{class}
\name{McmcdbWide-class}
\alias{McmcdbWide-class}
\title{MCMC Samples in long-format}
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
  \item{\code{parameters}}{\linkS4class{McmcParameters}.}
  \item{\code{chains}}{\linkS4class{McmcChains}.}
  \item{\code{iters}}{\linkS4class{McmcIters}.}
  \item{\code{parchains}}{\linkS4class{McmcFlatparChains}.}
  \item{\code{metadata}}{\code{list} with general data
  about the samples.}
  \item{\code{version}}{\code{character} version of
  \pkg{mcmcdb} with which the object was created} }
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
mcmcdb_iters(line_mcmcdbwide)
mcmcdb_parchains(line_mcmcdbwide)
mcmcdb_metadata(line_mcmcdbwide)
}

