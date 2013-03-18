\docType{class}
\name{McmcdbFlatparList-class}
\alias{McmcdbFlatparList}
\alias{McmcdbFlatparList-class}
\title{McmcdbFlatparList class}
\usage{
  McmcdbFlatparList(...)
}
\description{
  A list of \linkS4class{McmcdbFlatpar} objects.
}
\section{Extends}{
  \describe{ \item{\code{namedList}}{directly} }
}
\examples{
showClass("McmcdbFlatparList")
flatpars <-
  structure(mapply(function(x, y) {
   McmcdbFlatpar(pararray=x, index=y)
  }, "beta", 1:2, SIMPLIFY=FALSE),
           .Names = paste0("beta[", 1:2, "]"))
McmcdbFlatparList(flatpars)
}
\seealso{
  \linkS4class{McmcdbFlatpar}
}
\keyword{internal}

