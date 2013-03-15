\docType{class}
\name{McmcFlatparList-class}
\alias{McmcFlatparList}
\alias{McmcFlatparList-class}
\title{McmcFlatparList class}
\description{
  A list of \linkS4class{McmcFlatpar} objects.
}
\section{Extends}{
  \describe{ \item{\code{namedList}}{directly} }
}
\examples{
showClass("McmcFlatparList")
flatpars <-
  structure(mapply(function(x, y) {
   McmcFlatpar(pararray=x, index=y)
  }, "beta", 1:2, SIMPLIFY=FALSE),
           .Names = paste0("beta[", 1:2, "]"))
McmcFlatparList(flatpars)
}
\seealso{
  \linkS4class{McmcFlatpar}
}
\keyword{internal}

