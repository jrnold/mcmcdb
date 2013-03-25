\docType{methods}
\name{mcmdb_mcpar-methods}
\alias{mcmcdb_mcpar}
\alias{mcmcdb_mcpar,Mcmcdb-method}
\alias{mcmdb_mcpar-methods}
\title{Methods for function \code{mcmcdb_par}}
\arguments{
  \item{object}{Object}
}
\value{
  \code{data.frame} with columns \describe{
  \item{\code{chain_id}}{Chain id}
  \item{\code{n_iter}}{\code{integer}. Number of
  iterations} \item{\code{iter_start}}{\code{integer}.
  First iteration saved. Can be missing}
  \item{\code{iter_end}}{\code{integer}. Last iteration
  saved. Can be missing}
  \item{\code{iter_thin}}{\code{integer}. Thinning saved.
  Can be missing} }
}
\description{
  Get basic information about each chain: number of
  iterations, first iteration, last iteration, thinning.
}
\section{Methods}{
  \describe{ \item{\code{signature(object = "Mcmcdb")}}{} }
}
\examples{
data(line_samples)
mcmcdb_mcpar(line_samples)
}
\seealso{
  Other Mcmcdb methods: \code{\link{mcmcdb_flatpars}},
  \code{\link{mcmcdb_flatpars}},
  \code{\link{mcmcdb_flatpars-methods}},
  \code{\link{mcmcdb_pardims}},
  \code{\link{mcmcdb_pardims}},
  \code{\link{mcmcdb_pardims-methods}},
  \code{\link{mcmcdb_paridx}}, \code{\link{mcmcdb_paridx}},
  \code{\link{mcmcdb_paridx-methods}},
  \code{\link{mcmcdb_samples_chain_flatpars}},
  \code{\link{mcmcdb_samples_chain_flatpars}},
  \code{\link{mcmcdb_samples_chain_iter}},
  \code{\link{mcmcdb_samples_chain_iter}},
  \code{\link{mcmcdb_samples_chain_pararrays}},
  \code{\link{mcmcdb_samples_chain_pararrays}},
  \code{\link{mcmcdb_samples_flatpars_chain}},
  \code{\link{mcmcdb_samples_flatpars_chain}},
  \code{\link{mcmcdb_samples_pararrays_chain}},
  \code{\link{mcmcdb_samples_pararrays_chain}}
}

