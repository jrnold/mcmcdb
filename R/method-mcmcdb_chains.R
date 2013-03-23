#' @include class-McmcdbMem.R
#' @exportMethod mcmcdb_chains
NULL

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
#' then \code{\linkS4class{McmcdbChains}}, of the chains.
#' @aliases mcmcdb_chains,McmcdbMem-method
#' @family McmcdbWide methods
setMethod("mcmcdb_chains", "McmcdbMem",
            function(object, drop=TRUE) {
              if (drop) {
                object@chains[["chain_id"]]
              } else {
                object@chains
              }
            })
