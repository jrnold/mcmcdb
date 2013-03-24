\name{read_stan_csv}
\alias{read_stan_csv}
\title{Read csv output from command line Stan}
\usage{
  read_stan_csv(file)
}
\arguments{
  \item{file}{character. name of an output file produced by
  a STAN model.}
}
\value{
  An object of class \code{"\linkS4class{StanSamples}"}.
}
\description{
  Read csv files produced by Stan.
}
\details{
  This returns both the sample values and the metadata in
  the comments of the file. This function has been tested
  for the output of Stan 1.2.0.
}

