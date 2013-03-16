\name{McmcdbWide-method}
\alias{McmcdbWide}
\alias{McmcdbWide,matrix-method}
\alias{McmcdbWide,mcmc-method}
\alias{McmcdbWide,mcmc.list-method}
\alias{McmcdbWide-method}
\title{Create McmcdbWide objects}
\arguments{
  \item{x}{Numeric \code{matrix} with parameter sample
  values. Columns be named with the parameter names.}

  \item{parameters}{\linkS4class{McmcParameters} object or
  a \code{function} that returns a
  \linkS4class{McmcParameters} given \code{colnames(x)}.}

  \item{chains}{\linkS4class{McmcChains} object or
  \code{NULL}.}

  \item{iters}{\linkS4class{McmcIters} object or
  \code{NULL}.}

  \item{parchains}{\linkS4class{McmcParChains} object or
  \code{NULL}.}

  \item{metadata}{\code{list} with additional data about
  the MCMC samples.}
}
\value{
  An object of class \linkS4class{McmcdbWide}.
}
\description{
  Methods to create \linkS4class{McmcdbWide} objects.
}
\seealso{
  \linkS4class{McmcdbWide} for a definition of the class
}

