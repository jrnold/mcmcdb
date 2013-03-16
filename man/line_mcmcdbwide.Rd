\docType{data}
\name{line_mcmcdbwide}
\alias{line_mcmcdbwide}
\title{Test \code{McmcdbWide} Object}
\format{An object of class \code{\linkS4class{McmcdbWide}}. There are two parameter
arrays,
\describe{
\item{\code{beta}}{A vector of length 2}
\item{\code{tau}}{A scalar}
}}
\description{
  A \code{\link{McmcdbWide}} object used for examples in
  this package.
}
\details{
  The samples were generated using the following Jags
  model, \samp{ model { tau ~ dgamma(0.001, 0.001) for(i in
  1:N) { y[i] ~ dnorm(beta[1] + beta[2] * x[i], tau) } for
  (i in 1:2) { beta[i] ~ dnorm(0.0,1.0) } } }
}
\examples{
data(line_mcmcdbwide)
print(line_mcmcdbwide)
}
\keyword{datasets}

