\docType{class}
\name{McmcWide-class}
\alias{McmcWide-class}
\title{MCMC Samples in long-format}
\description{
  Mcmc samples stored as a matrix with (number of chains x
  number of iterations) rows and (number of flat
  parameters) columns.
}
\section{Slots}{
  \describe{ \item{\code{samples}}{\code{matrix}.}
  \item{\code{parameters}}{\linkS4class{McmcParameters}.}
  \item{\code{chains}}{\linkS4class{McmcChains}.}
  \item{\code{iters}}{\linkS4class{McmcIters}.}
  \item{\code{par_chains}}{\linkS4class{McmcParChains}.}
  \item{\code{metadata}}{\code{list} with general data
  about the samples.}
  \item{\code{version}}{\code{character} version of
  \pkg{mcmcdb} with which the object was created} }
}

