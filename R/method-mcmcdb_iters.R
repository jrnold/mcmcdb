#' @include class-McmcdbMem.R
#' @exportMethod mcmcdb_iters
NULL

setGeneric("mcmcdb_iters",
           function(object, ...) {
             standardGeneric("mcmcdb_iters")
           })

mcmcdb_iters.McmcdbMem <- 
          function(object, chain_id = NULL, iter = NULL, drop=TRUE) {
            if (is.null(chain_id) && is.null(iter)) {
              touse <- TRUE
            } else {
              touse <- ((is.null(chain_id)
                         | object@iters[["chain_id"]] %in% chain_id)
                        &
                        (is.null(iter)
                         | object@iters[["iter"]] %in% iter))
            }
            if (drop) {
              object@iters[touse, c("chain_id", "iter")]
            } else {
              object@iters[touse, ]
            }
          }

#' Get iteration data from Mcmcdb object
#'
#' @param object object
#' @param drop \code{logical}. If \code{TRUE}, only return
#' \code{chain_id} and \code{iter} columns. If \code{FALSE}, then
#' return all data related to the iterations (other than their values).
#' @param iter Select only iterations with an interation number in \code{iter}.
#' @param chain_id Select only iteration with a chain in \code{chain_id}.
#' @return An object of \linkS4class{McmcdbIters} with chains and iterations,
#' and associated data, if \code{drop=FALSE}.
#' @family get-methods
#' @aliases mcmcdb_iters,McmcdbMem-method
#' @family McmcdbMem methods
setMethod("mcmcdb_iters", "McmcdbMem", mcmcdb_iters.McmcdbMem)
