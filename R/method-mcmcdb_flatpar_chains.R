#' @include class-McmcdbWide.R
#' @exportMethod mcmcdb_flatpar_chains
NULL

setGeneric("mcmcdb_flatpar_chains",
           function(object, ...) {
             standardGeneric("mcmcdb_flatpar_chains")
           })

#' Get flat parameter, chains from an Mcmcdb object
#'
#' @param object object
#' @param drop \code{logical}. If \code{TRUE}, only return
#' \code{flatpar} and \code{} columns. If \code{FALSE}, then
#' return all data related to the iterations (other than their values).
#' @return An object of \linkS4class{McmcdbFlatparChains} with
#' flat parameters and chains.
#' @family get-methods
#' @aliases mcmcdb_flatpar_chains,McmcdbWide-method
#' @family McmcdbWide-methods
setMethod("mcmcdb_flatpar_chains", "McmcdbWide",
          function(object, drop=TRUE) {
            if (is.null(object@flatpar_chains)) {
              parameters <- names(object@parameters@flatpars)
              McmcdbFlatparChains(expand.grid(flatpar = parameters,
                                              chain_id=object@chains[ , c("chain_id")]))
            } else {
              if (drop) {
                object@flatpar_chains[ , c("flatpar", "chain_id")]
              } else {
                object@flatpar_chains
              }
            }
          })
