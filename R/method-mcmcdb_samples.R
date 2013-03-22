?#' @include package.R
#' @include class-McmcdbWide.R
NULL

#' Retrieve Sample data from Mcmcdb Object
#'
#'
setGeneric("mcmcdb_samples_long",
           function(object, ...) {
             standardGeneric("mcmcdb_samples_long")
           })

setMethod("mcmcdb_samples_long", "McmcdbWide",
          function(object, flatpars=NULL, pararrays=NULL, iter=NULL, chain_id=NULL) {
            ## TODO: skip this step
            iters <- select_iters(object, chain_id = chain_id, iter = iter)
            pars <- paramlist(object, flatpars=flatpars, pararrays=pararrays)
            x <- as.data.frame(object@samples[iters, pars])
            x[ , c("chain_id", "iter")] <- object@chains[iters, c("chain_id", "iter")]
            reshape(x, id.vars=c("chain_id", "iter"))
          })

setGeneric("mcmcdb_samples_flatpars",
           function(object, ...) {
             standardGeneric("mcmcdb_samples_flatpars")
           })

setMethod("mcmcdb_samples_flatpars", "McmcdbWide",
          function(object, ...) {
            subset_mcmcdb_wide(object, ...)
          })

setMethod("mcmcdb_samples_chains_flatpars", "McmcdbWide",
          function(object, ...) {
            pars <- paramlist(object, flatpars=flatpars,
                              pararrays=pararrays)
            f <- function(chain_id) {
              touse <- select_iters(object, chain_id = chain_id,
                                    iters = iters)
              x@samples[touse, pars]
            }
            llply(chain_id, f)
          })

setMethod("mcmcdb_samples_flatpars_chains", "McmcdbWide",
          function(object, ...) {
          })

paramlist <- function(x, flatpars=NULL, pararrays=NULL) {
  flatpars <- union(flatpars,
                    unlist(mcmcdb_parameters(x)[pararrays]))
  if (is.null(flatpars)) {
    flatpars <- TRUE
  }
  flatpars
}
          
select_iters <- function(x, chain_id = NULL, iter = NULL) {
  ## chains
  if (is.null(chain_id)) {
    ischain <- TRUE
  } else {
    ischain <- (x@iters[["chain_id"]] %in% chain_id)
  }
  # iterations
  if (is.null(iter)) {
    isiter <- TRUE
  } else {
    isiter <- (x@iters[["iter"]] %in% iter)
  }
  ischain & isiter
}

subset_mcmcdb_wide <- function(x, pararrays = NULL, flatpars = NULL,
                               chain_id = NULL, iter = NULL, drop=FALSE) {
  # Parameters
  rows <- select_iters(x, chain_id = unique(chain_id),
                       iter = unique(iter))
  x@samples[rows, flatpars, drop=drop]
}
