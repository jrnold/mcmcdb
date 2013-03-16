\name{bugs_to_stan_parnames}
\alias{bugs_to_stan_parnames}
\alias{stan_to_bugs_parnames}
\alias{stan_to_bugs_parnamse}
\title{Convert between BUGS/JAGS and Stan style flat parameter names}
\usage{
  bugs_to_stan_parnames(x)

  stan_to_bugs_parnames(x)
}
\arguments{
  \item{x}{\code{character} vector of flat parameter
  names.}
}
\value{
  \code{character} vector of the converted flat parameter
  names.
}
\description{
  Utility functions to convert flat parameter names between
  the BUGS (\code{"alpha[1,1]"}) and Stan style
  (\code{"alpha.1.1"}).
}
\examples{
stan_parnames <- c("alpha", "beta.1", "gamma.1.1")
bugs_parnames <- c("alpha", "beta[1]", "gamma[1,1]")
identical(bugs_to_stan_parnames(bugs_parnames), stan_parnames)
identical(stan_to_bugs_parnames(stan_parnames), bugs_parnames)
}

