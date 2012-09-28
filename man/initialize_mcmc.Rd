\name{initialize_mcmc}
\alias{initialize_mcmc}
\title{Initialize}
\usage{
  initialize_mcmc(.Object, data, start = 1,
    end = start + nrow(data) - 1,
    thin = (end - start + 1)/nrow(data))
}
\description{
  Initialize
}

