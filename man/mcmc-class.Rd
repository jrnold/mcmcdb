\docType{class}
\name{mcmc-class}
\alias{mcmc-class}
\title{Markov Chain Monte Carlo Objects}
\description{
  S4 class which wraps the \pkg{coda} S3 class
  \code{\link[coda]{mcmc}}.
}
\section{Slots}{
  \describe{ \item{\code{.Data}:}{Object of class
  \code{"matrix"}. Parameter matrix.}
  \item{\code{mcpar}:}{Object of class \code{"numeric"} of
  length 3: start, end, and thin of the iterations. }
  \item{\code{.S3Class}:}{This class extends an S3 class.}
  }
}

\section{Extends}{
  Class \code{"\linkS4class{oldClass}"}, directly. Class
  \code{"\linkS4class{matrix}"}, from data part.
}
\seealso{
  \code{\link[coda]{mcmc}}
}

