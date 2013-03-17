\docType{class}
\name{McmcdbPararrayList-class}
\alias{McmcdbPararrayList}
\alias{McmcdbPararrayList-class}
\title{McmcdbPararrayList Class}
\usage{
  McmcdbPararrayList(...)
}
\description{
  A list of \linkS4class{McmcdbPararray} objects.
}
\section{Extends}{
  \describe{ \item{\code{namedList}}{directly.} }
}
\examples{
showClass("McmcdbPararrayList")
mcmcpararrays <-
 list(beta = McmcdbPararray(flatpars=paste0("beta[", 1:2L, "]"), dim=2L),
      alpha = McmcdbPararray(flatpars="alpha", dim=1L))
McmcdbPararrayList(mcmcpararrays)
}
\seealso{
  \code{\link{McmcdbPararray}}
}
\keyword{internal}

