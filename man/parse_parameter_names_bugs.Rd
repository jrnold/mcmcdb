\name{parse_parameter_names_bugs}
\alias{parse_parameter_names_bugs}
\title{Parse flat parameter names produced by BUGS}
\usage{
  parse_parameter_names_bugs(x)
}
\arguments{
  \item{x}{\code{character} vector of character names.}
}
\description{
  Bugs parameter names have the form
  "{parameter}[{index}]". E.g. the first row, third column
  of a matrix parameter "beta" is named "beta[1,2]".
}

