\name{mcmcdb_wide_from_stan}
\alias{mcmcdb_wide_from_stan}
\title{Create McmdbWide from Stan csv files}
\usage{
  mcmcdb_wide_from_stan(file, init = NULL,
    model_data = NULL, model_name = NULL,
    model_code = NULL)
}
\arguments{
  \item{file}{\code{character}. Name of an output file
  produced by a STAN model.}

  \item{init}{\code{list} or \code{NULL}. List of initial
  values.}

  \item{model_data}{\code{list} or \code{NULL}. List of
  model data.}

  \item{model_name}{\code{character} Model name.}

  \item{model_code}{\code{character} Stan model code.}
}
\value{
  An object of class \code{"McmcdbWide"}
}
\description{
  This returns both the sample values and the metadata in
  the comments of the file. This function has been tested
  for the output of Stan 1.2.0.
}

