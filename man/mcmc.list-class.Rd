\docType{class}
\name{mcmc.list-class}
\alias{mcmc.list-class}
\title{Markov Chain Monte Carlo Object List}
\description{
  S4 class which wraps the \bold{coda} S3 class
  \code{\link[coda]{mcmc.list}}. This is simply a list of
  \code{\link{mcmc}} object.
}
\section{Slots}{
  \describe{ \item{\code{.Data}:}{Object of class
  \code{"list"} of \code{"mcmc"} objects. }
  \item{\code{.S3Class}:}{Object of class
  \code{"character"}, name of S3 class this extends. } }
}

\section{Extends}{
  Class \code{"\linkS4class{list}"}, from data part.
}
\seealso{
  \code{\link[coda]{mcmc}}
}

