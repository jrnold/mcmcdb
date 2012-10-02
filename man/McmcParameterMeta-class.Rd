\name{McmcParameterMeta}
\alias{McmcParameterMeta}
\alias{McmcParameterMeta-class}
\title{MCMC Parameter Metadata}
\description{
  This class stores the mapping between the names of MCMC
  parameters in their flat representation (as returned by
  samplers like BUGS) and their array representation in the
  model. This class facilitates transforming parameters
  from their flat to their array representation.
}
\section{Slots}{
  \describe{ \item{\code{parameters}}{named character
  vector. Values are the names of the array parameters;
  names are the names of the flat parameters.}
  \item{\code{skeleton}}{List of arrays. Each element has
  the name of the array parameter, and an array value of
  the proper shape of that parameter.}
  \item{\code{indices}}{List of matrices, one for each
  array parameter. Each row the matrix is a has a rowname
  of a flat parameter and values that are the index of flat
  parameter in the array parameter.} }
}

