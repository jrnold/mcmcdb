#' @include McmcdbWide.R
NULL

#' @export
setGeneric("mcmcdb_chains",
           function(object, ...) {
             standardGeneric("mcmcdb_chains")
           })

#' Get Chains data from Mcmcdb object
#'
#' @param object object
#' @param drop \code{logical} Include data related to the chains
#' or just
#' @family get-methods
#' @return If \code{drop=TRUE} then \code{integer}, if \code{drop=FALSE},
#' then \code{\linkS4class{McmcChains}}.
#' @seealso \code{\linkS4class{McmcdbWide}}
#' @aliases mcmcdb_chains,McmcdbWide-method
setMethod("mcmcdb_chains", "McmcdbWide",
            function(object, drop=TRUE) {
              if (drop) {
                object@chains[["chain_id"]]
              } else {
                object@chains
              }
            })

#' @export
setGeneric("mcmcdb_parameters",
          function(object, ...) {
            standardGeneric("mcmcdb_parameters")
          })

#' Get parameters data from Mcmcdb object
#'
#' @param object object
#' @return \linkS4class{McmcParameters}
#' @family get-methods
#' @seealso \code{\linkS4class{McmcdbWide}}
#' @aliases mcmcdb_parameters,McmcdbWide-method
setMethod("mcmcdb_parameters", "McmcdbWide",
            function(object) {
              object@parameters
            })


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
#' @return \linkS4class{McmcIters}
#' @family get-methods
#' @aliases mcmcdb_iters,McmcdbWide-method
#' @seealso \code{\linkS4class{McmcdbWide}}
setMethod("mcmcdb_iters", "McmcdbWide",
          function(object, drop=TRUE) {
            if (drop) {
              object@iters[ , c("chain_id", "iter")]
            } else {
              object@iters
            }
          })


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
              McmcFlatparChains(expand.grid(flatpar = parameters,
                                        chain_id=object@chains[ , c("chain_id")]))
            } else {
              if (drop) {
                object@parchains[ , c("flatpar", "chain_id")]
              } else {
                object@parchains
              }
            }
          })


#' @export
setGeneric("mcmcdb_metadata",
           function(object, ...) {
             standardGeneric("mcmcdb_metadata")
           })

#' Get metadata from an Mcmcdb object
#'
#' @param object object
#' @return \code{list}
#' @aliases mcmcdb_metadata,McmcdbWide-method
#' @seealso \code{\linkS4class{McmcdbWide}}
setMethod("mcmcdb_metadata", "McmcdbWide",
          function(object) object@metadata)

