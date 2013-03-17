#' @include class-McmcdbWide.R
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