#' @include package.R
#' @include class-McmcdbWide.R
NULL

## #' Mcmcdb Object samples (iters forms)
## setGeneric("mcmcdb_samples_iters",
##            function(object, ...) {
##              standardGeneric("mcmcdb_samples_iters")
##            })

## #' Mcmcdb Object samples (Long form)
## setGeneric("mcmcdb_samples_long",
##            function(object, ...) {
##              standardGeneric("mcmcdb_samples_long")
##            })

## setMethod("mcmcdb_samples_long", "McmcdbWide",
##           function(object, flatpars=NULL, pararrays=NULL, iter=NULL,
##                    chain_id=NULL) {
##             ## TODO: skip this step
##             iters <- select_iters(object, chain_id = chain_id, iter = iter)
##             pars <- paramlist(object, flatpars=flatpars, pararrays=pararrays)
##             x <- as.data.frame(object@samples[iters, pars])
##             x[ , c("chain_id", "iter")] <-
##               object@chains[iters, c("chain_id", "iter")]
##             reshape(x, id.vars=c("chain_id", "iter"))
##           })


## #' Mcmcdb Object samples (Flatpars form)
## setGeneric("mcmcdb_samples_flatpars",
##            function(object, ...) {
##              standardGeneric("mcmcdb_samples_flatpars")
##            })

## setMethod("mcmcdb_samples_flatpars", "McmcdbWide",
##           function(object, ...) {
##             subset_mcmcdb_wide(object, ...)
##           })

## #' Mcmcdb Object samples (Chains,Flatpars form)
## setMethod("mcmcdb_samples_chains_flatpars", "McmcdbWide",
##           function(object, ...) {
##             pars <- paramlist(object, flatpars=flatpars,
##                               pararrays=pararrays)
##             f <- function(chain_id) {
##               touse <- select_iters(object, chain_id = chain_id,
##                                     iters = iters)
##               x@samples[touse, pars]
##             }
##             llply(chain_id, f)
##           })

## setMethod("mcmcdb_samples_flatpars_chains", "McmcdbWide",
##           function(object, ...) {
##           })
