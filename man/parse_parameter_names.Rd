\name{parse_parameter_names_default}
\alias{parse_parameter_names_bugs}
\alias{parse_parameter_names_default}
\alias{parse_parameter_names_stan}
\title{Parse MCMC parameter names}
\usage{
  parse_parameter_names_default(x)

  parse_parameter_names_stan(x)

  parse_parameter_names_bugs(x)
}
\arguments{
  \item{x}{\code{character} vector of character names.}
}
\value{
  \code{data.frame} with rownames equal to \code{x} and two
  columns \describe{ \item{\code{index}}{Comma seperated
  string of the index of the flat parameter in the array
  parameter. E.g. If "beta.1.2" is the first column, second
  row of "beta", then this should be equal to "1,2".}
  \item{\code{name}}{Name of the array parameter. E.g. If
  "beta.1.2" is the first column, second row of "beta",
  then this should be equal to "beta".} }
}
\description{
  Parse MCMC parameter names
}

