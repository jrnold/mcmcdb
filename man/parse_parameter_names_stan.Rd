\name{parse_parameter_names_stan}
\alias{parse_parameter_names_stan}
\title{Parse flat parameter names produced by Stan}
\usage{
  parse_parameter_names_stan(x)
}
\arguments{
  \item{x}{\code{character} vector of character names.}
}
\description{
  Bugs parameter names have the form "{parameter}.{index}",
  with the index values separated by periods. E.g. the
  first row, third column of a matrix parameter "beta" is
  named "beta.1.2". Stan parameters can only use the
  characters [A-Za-z_].
}

