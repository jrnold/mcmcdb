#' @include package.R
#' @include class-McmcdbWide.R
NULL

#' @rdname mcmcdb_samples_flatpars-methods
#' @title Mcmcdb Object samples (Flatpars form)
#'
#' @description Return samples from a Mcmcdb
#' object in a matrix (iterations x flatpars).
#' 
#' @return \code{matrix}. Columns are the flatpars.
#' Rows are the iterations (from all chains).
setGeneric("mcmcdb_samples_flatpars",
           function(object, ...) {
             standardGeneric("mcmcdb_samples_flatpars")
           })

#' @rdname mcmcdb_samples_flatpars-methods
#' @aliases mcmcdb_samples_flatpars,McmcdbWide-method
#' @family McmcdbWide methods
setMethod("mcmcdb_samples_flatpars", "McmcdbWide",
          function(object, flatpars=NULL, pararrays=NULL, iter=NULL,
                   chain_id=NULL, drop=FALSE) {
            mcmcdb_wide_subset(object,
                               flatpars=flatpars, pararrays=pararrays,
                               iter=iter, chain_id=chain_id, drop=drop)
          })