#' @include package.R
#' @include class-McmcdbWide.R
#' @exportMethod mcmcdb_rename_chain
NULL


#' @rdname mcmcdb_rename_chain-methods
#' @title Methods for function \code{mcmcdb_rename_chain}
#'
#' @description Change \code{chain_id} values in a \code{Mcmcdb} object.
#'
#' @param x  The object to modify.
#' @param from The vector of chain ids to replace
#' @param to The vector of chain ids replacement values
#' @return An object of the same class as \code{x}.
setGeneric("mcmcdb_rename_chain",
           function(object, from, to, ...) {
             standardGeneric("mcmcdb_rename_chain")
           })

mcmcdb_rename_chain.McmcdbWide <- function(object, from, to) {
  from <- as.integer(from)
  to <- as.integer(to)
  object@chains[["chain_id"]] <-
    mapvalues(object@chains[["chain_id"]], from, to)
  object@iters[["chain_id"]] <-
    mapvalues(object@iters[["chain_id"]], from, to)
  object@flatpar_chains[["chain_id"]] <-
    mapvalues(object@flatpar_chains[["chain_id"]], from, to)
  validObject(object)
  object
}

#' @rdname mcmcdb_rename_chain-methods
#' @aliases mcmcdb_rename_chain,McmcdbWide-method
setMethod("mcmcdb_rename_chain",
          c(object="McmcdbWide", from="ANY", to="ANY"),
          mcmcdb_rename_chain.McmcdbWide)
