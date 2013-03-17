#' @include class-McmcdbWide.R
NULL

#' @export
setGeneric("mcmcdb_parchains",
           function(object, ...) {
             standardGeneric("mcmcdb_parchains")
           })

#' Get parameter-chain data from an Mcmcdb object
#'
#' @param object object
#' @param drop \code{logical}. If \code{TRUE}, only return
#' \code{flatpar} and \code{} columns. If \code{FALSE}, then
#' return all data related to the iterations (other than their values).
#' @return \linkS4class{McmcIters}
#' @family get-methods
#' @aliases mcmcdb_parchains,McmcdbWide-method
#' @seealso \code{\linkS4class{McmcdbWide}}
setMethod("mcmcdb_parchains", "McmcdbWide",
          function(object, drop=TRUE) {
            if (is.null(object@parchains)) {
              parameters <- names(object@parameters@flatpars)
              McmcdbFlatparChains(expand.grid(flatpar = parameters,
                                        chain_id=object@chains[ , c("chain_id")]))
            } else {
              if (drop) {
                object@parchains[ , c("flatpar", "chain_id")]
              } else {
                object@parchains
              }
            }
          })
