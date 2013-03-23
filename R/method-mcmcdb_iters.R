#' @include class-McmcdbMem.R
#' @exportMethod mcmcdb_iters
NULL

setGeneric("mcmcdb_iters",
           function(object, ...) {
             standardGeneric("mcmcdb_iters")
           })

#' Get iteration data from Mcmcdb object
#'
#' @param object object
#' @param drop \code{logical}. If \code{TRUE}, only return
#' \code{chain_id} and \code{iter} columns. If \code{FALSE}, then
#' return all data related to the iterations (other than their values).
#' @return An object of \linkS4class{McmcdbIters} with chains and iterations,
#' and associated data, if \code{drop=FALSE}.
#' @family get-methods
#' @aliases mcmcdb_iters,McmcdbMem-method
#' @family McmcdbMem methods
setMethod("mcmcdb_iters", "McmcdbMem",
          function(object, drop=TRUE) {
            if (drop) {
              object@iters[ , c("chain_id", "iter")]
            } else {
              object@iters
            }
          })

