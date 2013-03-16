#' @include McmcdbWide.R

#' @export
setGeneric("mcmcdb_get_chains",
           function(object, ...) {
             standardGeneric("mcmcdb_get_chains")
           })

#' Get Chains data from Mcmcdb object
#'
#' @param object object
#' @param drop \code{logical} Include data related to the chains
#' or just
#' @family get-methods
#' @return If \code{drop=TRUE} then \code{integer}, if \code{drop=FALSE},
#' then \code{\linkS4class{McmcChains}}.
#' @aliases mcmcdb_get_chains,McmcdbWide-method
setMethod("mcmcdb_get_chains", "McmcdbWide",
            function(object, drop=TRUE) {
              if (drop) {
                object@chains[["chain_id"]]
              } else {
                object@chains
              }
            })

#' @export
setGeneric("mcmcdb_get_parameters",
          function(object, ...) {
            standardGeneric("mcmcdb_get_parameters")
          })

#' Get parameters data from Mcmcdb object
#'
#' @param object object
#' @return \linkS4class{McmcParameters}
#' @family get-methods
#' @aliases mcmcdb_get_parameters,McmcdbWide-method
setMethod("mcmcdb_get_parameters", "McmcdbWide",
            function(object) {
              object@parameters
            })


#' @export
setGeneric("mcmcdb_get_iters",
           function(object, ...) {
             standardGeneric("mcmcdb_get_iters")
           })

#' Get iteration data from Mcmcdb object
#'
#' @param object object
#' @param drop \code{logical}. If \code{TRUE}, only return
#' \code{chain_id} and \code{iter} columns. If \code{FALSE}, then
#' return all data related to the iterations (other than their values).
#' @return \linkS4class{McmcIters}
#' @family get-methods
#' @aliases mcmcdb_get_iters,McmcdbWide-method
setMethod("mcmcdb_get_iters", "McmcdbWide",
          function(object, drop=TRUE) {
            if (drop) {
              objec@iters[ , c("chain_id", "iter")]
            } else {
              object@iters
            }
          })


#' @export
setGeneric("mcmcdb_get_parchains",
           function(object, ...) {
             standardGeneric("mcmcdb_get_parchains")
           })

#' Get parameter-chain data from an Mcmcdb object
#'
#' @param object object
#' @param drop \code{logical}. If \code{TRUE}, only return
#' \code{flatpar} and \code{} columns. If \code{FALSE}, then
#' return all data related to the iterations (other than their values).
#' @return \linkS4class{McmcIters}
#' @family get-methods
#' @aliases mcmcdb_get_parchains,McmcdbWide-method
setMethod("mcmcdb_get_parchains", "McmcdbWide",
          function(object, drop=TRUE) {
            if (is.null(object@parchains)) {
              parameters <- names(foo@parameters@flatpars)
              McmcIters(expand.grid(objectparameters,
                                    object@chains[ , c("chain_id", "iter")]))
            } else {
              if (drop) {
                objec@iters[ , c("flatpar", "chain_id", "iter")]
              }
            }
            if (drop) {
              
            } else {
              object@iters
            }
          })


#' @export
setGeneric("mcmcdb_get_metadata",
           function(object, ...) {
             standardGeneric("mcmcdb_get_metadata")
           })

#' Get metadata from an Mcmcdb object
#'
#' @param object object
#' @return \code{list}
#' @aliases mcmcdb_get_metadata,McmcdbWide-method
setMethod("mcmcdb_get_metadata", "McmcdbWide",
          function(object) object@metadata)

