#' @include class-McmcdbWide.R
NULL

#' @export
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
#' @return \linkS4class{McmcdbIters}
#' @family get-methods
#' @aliases mcmcdb_iters,McmcdbWide-method
#' @seealso \code{\linkS4class{McmcdbWide}}
#' @family McmcdbWide methods
setMethod("mcmcdb_iters", "McmcdbWide",
          function(object, drop=TRUE) {
            if (drop) {
              object@iters[ , c("chain_id", "iter")]
            } else {
              object@iters
            }
          })

