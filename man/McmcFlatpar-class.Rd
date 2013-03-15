\docType{class}
\name{McmcFlatpar-class}
\alias{McmcFlatpar}
\alias{McmcFlatpar-class}
\title{McmcFlatpar class}
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
showClass("McmcFlatpar")
McmcFlatpar(pararray="beta", index=c(1L, 1L))
}
\seealso{
  \linkS4class{McmcFlatparList} for a list of
  \code{McmcFlatpar} objects.
}

