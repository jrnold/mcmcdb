#' @include package.R
#' @include class-McmcdbWide.R
#' @exportMethod mcmcdb_drop_parameter
NULL

#' @rdname mcmcdb_drop_parameter-methods
#' @docType methods
#' @title Methods for function \code{mcmcdb_drop_parameter}
#'
#' @description Drop specified parameter arrays from a \code{Mcmcdb} object.
#'
#' @param object An object containing MCMC samples
#' @param parameter Names of the parameter arrays
#' @return An object of the same class as \code{object}.
#' @family McmcdbWide methods
setGeneric("mcmcdb_drop_parameter",
           function(object, parameter, ...) {
             standardGeneric("mcmcdb_drop_parameter")
           })

mcmcdb_drop_parameter.McmcdbWide <- function(object, parameter) {
  parameters_from <- names(object@parameters)
  flatpars_drop <- as.character(object@parameters[parameter])
  parameters_to <- setdiff(parameters_from, parameter)
  
  object@parameters <- object@parameters[parameters_to]

  flatpars_chains_keep <- ! object@flatpar_chains[["flatpar"]] %in% flatpars_drop
  object@flatpar_chains <- object@flatpar_chains[flatpars_chains_keep, ]
  object@flatpar_chains[["flatpar"]] <- factor(object@flatpar_chains$flatpar)
  
  samples_keep <- ! colnames(object@samples) %in% flatpars_drop
  object@samples <- object@samples[ , samples_keep, drop=FALSE]

  validObject(object)
  object
}

#' @rdname mcmcdb_drop_parameter-methods
#' @aliases mcmcdb_drop_parameter,McmcdbWide,character-method
setMethod("mcmcdb_drop_parameter",
          c(object = "McmcdbWide", parameter = "character"),
          mcmcdb_drop_parameter.McmcdbWide)
