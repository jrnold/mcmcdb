#' @include package.R
#' @include class-McmcdbWide.R
#' @exportMethod mcmcdb_drop_pararray
NULL

#' @rdname mcmcdb_drop_pararray-methods
#' @docType method
#' @title Methods for function \code{mcmcdb_drop_pararray}
#'
#' @description Drop specified parameter arrays from a \code{Mcmcdb} object.
#'
#' @param object An object containing MCMC samples
#' @param pararray Names of the parameter arrays
#' @return An object of the same class as \code{object}.
#' @family McmcdbWide methods
setGeneric("mcmcdb_drop_pararray",
           function(object, pararray, ...) {
             standardGeneric("mcmcdb_drop_pararray")
           })

mcmcdb_drop_pararray.McmcdbWide <- function(object, pararray) {
  pararrays_from <- names(object@parameters)
  flatpars_drop <- as.character(object@parameters[pararray])
  pararrays_to <- setdiff(pararrays_from, pararray)
  
  object@parameters <- object@parameters[pararrays_to]

  flatpars_chains_keep <- ! object@flatpar_chains[["flatpar"]] %in% flatpars_drop
  object@flatpar_chains <- object@flatpar_chains[flatpars_chains_keep, ]
  object@flatpar_chains[["flatpar"]] <- factor(object@flatpar_chains$flatpar)
  
  samples_keep <- ! colnames(object@samples) %in% flatpars_drop
  object@samples <- object@samples[ , samples_keep, drop=FALSE]

  validObject(object)
  object
}

#' @rdname mcmcdb_drop_pararray-methods
#' @aliases mcmcdb_drop_pararray,McmcdbWide,character-method
setMethod("mcmcdb_drop_pararray",
          c(object = "McmcdbWide", pararray = "character"),
          mcmcdb_drop_pararray.McmcdbWide)
