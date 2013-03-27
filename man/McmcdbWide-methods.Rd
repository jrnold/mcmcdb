\name{McmcdbWide-methods}
\alias{McmcdbWide}
\alias{McmcdbWide,data.frame-method}
\alias{McmcdbWide,matrix-method}
\alias{McmcdbWide,mcmc-method}
\alias{McmcdbWide,mcmc.list-method}
\alias{McmcdbWide,stanfit-method}
\alias{McmcdbWide-methods}
\title{Create \code{McmcdbWide} objects}
\arguments{
  \item{x}{An object for which a method is available.}
}
\value{
  An object of class \code{\linkS4class{McmcdbWide}}
  objects.
}
\description{
  Create \code{\linkS4class{McmcdbWide}} objects.
}
\examples{
\dontrun{
# Convert stanfit object
library(rstan)
scode <- "
     parameters {
       real y[2];
     }
     model {
       y[1] ~ normal(0, 1);
       y[2] ~ double_exponential(0, 2);
     }
     "
fit1 <- stan(model_code = scode, iter = 10, verbose = FALSE)
fit2 <- McmcdbWide(fit1)
}
}

