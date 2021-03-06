#' @include class-Mcmcdb.R
#' @include method-mcmcdb_iters.R
#' @include method-mcmcdb_samples_flatpars.R
#' @exportMethod mcmcdb_resample
NULL


#' @rdname mcmcdb_resample
#' @docType methods
#' @aliases mcmcdb_resample
#' @aliases mcmcdb_resample-methods
#' @title Resample from a MCMC Samples
#'
#' @description Draw samples from a normal or t-distribution approximation
#' of the posterior distribution.
#'
#' @param object object with flat parameters.
#' @param n Number of samples to draw.
#' @param replace Sample with replacement?
setGeneric("mcmcdb_resample",
           function(object, ...) standardGeneric("mcmcdb_resample"))

mcmcdb_resample.Mcmcdb <- function(object, n = 1, replace = FALSE,
                                   chain_id = NULL, iter = NULL,
                                   parameters = NULL,
                                   flatten = TRUE) {
  chains <- mcmcdb_iters(object, chain_id = chain_id, iter = iter, drop = TRUE)
  i <- sample.int(nrow(chains), size = n, replace = replace)
  chains <- as(chains[i, , drop=FALSE], "data.frame")

  if (flatten) {
    maply(chains, function(chain_id, iter) {
      mcmcdb_samples_flatpars(object, chain_id = chain_id, iter = iter,
                              parameters = parameters)
    })
  } else {
    mlply(chains, function(chain_id, iter) {
      mcmcdb_samples_iter(object, chain_id = chain_id,
                          iter = iter, parameters = parameters)[[1]]
    })
  }    
}

#' @rdname mcmcdb_resample
#' @aliases mcmcdb_resample,Mcmcdb-method
setMethod("mcmcdb_resample", "Mcmcdb", mcmcdb_resample.Mcmcdb)
