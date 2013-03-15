\docType{class}
\name{McmcPararrayList-class}
\alias{McmcPararrayList}
\alias{McmcPararrayList-class}
\title{McmcPararrayList Class}
\usage{
  McmcPararrayList(...)
}
\description{
  A list of \linkS4class{McmcPararray} objects.
}
\section{Extends}{
  \describe{ \item{\code{namedList}}{directly.} }
}
\examples{
showClass("McmcPararrayList")
mcmcpararrays <-
 list(beta = McmcPararray(flatpars=paste0("beta[", 1:2L, "]"), dim=2L),
      alpha = McmcPararray(flatpars="alpha", dim=1L))
McmcPararrayList(mcmcpararrays)
}
\seealso{
  \code{\link{McmcPararray}}
}
\keyword{internal}

