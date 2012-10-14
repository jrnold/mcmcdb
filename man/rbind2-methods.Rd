\name{rbind2-methods}
\alias{rbind2-methods}
\alias{rbind2,mcmc.list,missing-method}
\title{Methods for function \code{rbind2} for MCMC samples}
\description{
  Methods for function \code{rbind2} for MCMC samples
}
\section{Methods}{
  \describe{
  \item{\code{signature(x="mcmc.list",y="missing")}}{ Binds
  all \code{mcmc} objects in a \code{mcmc.list} into a
  single matrix. This returns a \code{matrix} instead of
  \code{mcmc} since the start/end/thin values would no
  longer make sense.} }
}

