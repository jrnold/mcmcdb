\docType{methods}
\name{mcmc.list-methods}
\alias{mcmc.list}
\alias{mcmc.list,ANY-method}
\alias{mcmc.list,matrix-method}
\alias{mcmc.list,mcmc-method}
\alias{mcmc.list-methods}
\title{Create \code{mcmc.list} object}
\arguments{
  \item{x}{Object with MCMC samples.}

  \item{...}{Arguments passed to \code{\link{mcmc}}.}
}
\description{
  Create \code{mcmc.list} object
}
\section{Details}{
  Unlike \code{\link[coda]{mcmc.list}} this does not
  enforce equal sample sizes.
}
\seealso{
  \code{\link[coda]{mcmc}}
}
\keyword{methods}

