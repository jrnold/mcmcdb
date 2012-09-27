\name{gelman_diag_split}
\alias{gelman_diag_split}
\title{Gelman-Rubin Diagnostic with a Split Single Chain}
\arguments{
  \item{x}{Object containing the MCMC samples.

  The single chain is split in half, and then
  \code{gelman_diag} is run on the two halves.}
}
\description{
  Gelman-Rubin Diagnostic with a Split Single Chain
}
\references{
  Stan Manual, Section 27.2.
}

