\docType{methods}
\name{gelmanDiagSplit}
\alias{gelmanDiagSplit}
\alias{gelmanDiagSplit,list-method}
\alias{gelmanDiagSplit,matrix-method}
\alias{gelmanDiagSplit,mcmc-method}
\alias{gelmanDiagSplit-method}
\alias{gelmanDiagSplit,numeric-method}
\title{Gelman-Rubin Diagnostic with a Split Single Chain}
\arguments{
  \item{x}{Object containing the MCMC samples.}
}
\description{
  The single chain is split in half, and then
  \code{gelman_diag} is run on the two halves.
}
\references{
  Stan Manual, Section 27.2.
}
\keyword{methods}

