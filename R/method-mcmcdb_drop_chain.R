#' @include package.R
#' @include class-McmcdbWide.R
#' @exportMethod mcmcdb_drop_chain
NULL

#' @rdname mcmcdb_drop_chain-methods
#' @docType methods
#' @title Methods for function \code{mcmcdb_drop_chain}
#'
#' @description Drop specified chains from a \code{Mcmcdb} object.
#'
#' @param object An object containing MCMC samples
#' @param chain_id Chains to drop.
#' @return An object of the same class as \code{object}.
#' @family McmcdbWide methods
setGeneric("mcmcdb_drop_chain",
           function(object, chain_id, ...) {
             standardGeneric("mcmcdb_drop_chain")
           })

mcmcdb_drop_chain.McmcdbWide <- function(object, chain_id) {
  iters_keep <- ! mcmcdb_wide_select_iters(object, chain_id = chain_id)
  object@samples <- object@samples[iters_keep, ]
  object@iters <- object@iters[iters_keep, ]

  flatpar_chains_keep <- ! object@flatpar_chains[["chain_id"]] %in% chain_id
  object@flatpar_chains <- object@flatpar_chains[flatpar_chains_keep, ]

  chains_keep <- ! object@chains[["chain_id"]] %in% chain_id
  object@chains <- object@chains[chains_keep, drop=FALSE]
  validObject(object)
  object
}

#' @rdname mcmcdb_drop_chain-methods
#' @aliases mcmcdb_drop_chain,McmcdbWide,integer-method
setMethod("mcmcdb_drop_chain",
          c(object = "McmcdbWide", chain_id = "integer"),
          mcmcdb_drop_chain.McmcdbWide)

#' @rdname mcmcdb_drop_chain-methods
#' @aliases mcmcdb_drop_chain,McmcdbWide,numeric-method
setMethod("mcmcdb_drop_chain",
          c(object = "McmcdbWide", chain_id = "numeric"),
          function(object, chain_id) {
            callGeneric(object, as.integer(chain_id))
          })
