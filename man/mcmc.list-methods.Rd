\name{mcmc.list-methods}
\alias{mcmc.list}
\alias{mcmc.list,ANY-method}
\alias{mcmc.list,matrix-method}
\alias{mcmc.list,mcmc-method}
\alias{mcmc.list-methods}
\title{Create mcmc.list}
\arguments{
  \item{x}{Object with MCMC samples.}

  \item{...}{Arguments passed to \code{\link{mcmc}}.}
}
\description{
  Create mcmc.list
}
\section{Details}{
  Unlike \code{\link[coda]{mcmc.list}} this does not
  enforce equal sample sizes.
}
\seealso{
  \code{\link[coda]{mcmc}}
}

