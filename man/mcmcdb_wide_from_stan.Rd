\name{mcmcdb_wide_from_stan}
\alias{mcmcdb_wide_from_stan}
\title{Create McmdbWide from Stan csv}
\usage{
  mcmcdb_wide_from_stan(file)
}
\arguments{
  \item{file}{character. Name of an output file produced by
  a STAN model.}
}
\value{
  An object of class \code{"McmcdbWide"}
}
\description{
  Create McmdbWide from Stan csv
}
\details{
  This returns both the sample values and the metadata in
  the comments of the file. This function has been tested
  for the output of Stan 1.2.0.
}

