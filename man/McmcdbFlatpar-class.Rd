\docType{class}
\name{McmcdbFlatpar-class}
\alias{McmcdbFlatpar}
\alias{McmcdbFlatpar-class}
\title{McmcdbFlatpar class}
\usage{
  McmcdbFlatpar(...)
}
\description{
  Class representing attributes of a flattened MCMC
  parameter: the name of its parameter array and its index
  in that array.
}
\section{Slots}{
  \describe{ \item{\code{pararray}}{\code{"character"}.
  Name of parameter array}
  \item{\code{index}}{\code{"index"}. Index of parameter} }
}
\examples{
# beta[1,1]
showClass("McmcdbFlatpar")
McmcdbFlatpar(pararray="beta", index=c(1L, 1L))
}
\seealso{
  \linkS4class{McmcdbFlatparList} for a list of
  \code{McmcdbFlatpar} objects.
}

