\name{mcmcdb_wide_from_stan}
\alias{mcmcdb_wide_from_stan}
\title{Read STAN output}
\usage{
  mcmcdb_wide_from_stan(file)
}
\arguments{
  \item{file}{character. name of an output file produced by
  a STAN model.}
}
\value{
  A \code{data.frame} with attributes \describe{
  \item{\code{header}}{A \code{list} containing header
  information} \item{\code{rejected}}{A \code{logical}
  vector indicating whether the iteration was a rejected
  sample (for samples only)} \item{\code{warmup}}{A
  \code{logical} vector indicating whether the iteration
  was in the warmup.} }
}
\description{
  Read csv files produced by Stan.
}
\details{
  This returns both the sample values and the metadata in
  the comments of the file. This function has been tested
  for the output of Stan 1.2.0.
}

