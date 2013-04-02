#' @include package.R
#' @include class-McmcdbWide.R
#' @include method-mcmcdb_chains.R
#' @include mcmc_parparser.R
NULL

#' @rdname mcmcdb_add_parameters-methods
#' @title Methods for function \code{mcmcdb_add_parameters}
#'
#' @param object Object to add parameters to.
#' @param value Parameter values to add to \code{object}.
#' @param parameters An object of class \code{McmcdbParameters}.
#' @param flatpar_chains An object of class \code{McmcdbFlatparChains},
#'   containing information about each flat parameter for each chain,
#'   such as initial values.
setGeneric("mcmcdb_add_parameters",
           function(object, value, ...) {
             standardGeneric("mcmcdb_add_parameters")
           })


mcmcdb_add_parameters.McmcdbWide.matrix <-
  function(object, value, parameters = McmcdbParameters(colnames(value)),
           flatpar_chains = NULL) {
    object@parameters <- c(object@parameters, parameters)
    object@samples <- cbind(object@samples, value)
    if (is.null(flatpar_chains)) {
      chains <- mcmcdb_chains(object, drop=TRUE)
      flatpar_chains <-
        data.frame(expand.grid(flatpar = colnames(value),
                               chain_id = chains),
                   init = NA_real_)
    }
    object@flatpar_chains <-
      rbind2(object@flatpar_chains, flatpar_chains)
    validObject(object)
    object
  }

#' @rdname mcmcdb_add_parameters-methods
#' @aliases mcmcdb_add_parameters,McmcdbWide,matrix-method
setMethod("mcmcdb_add_parameters",
          c(object = "McmcdbWide", value = "matrix"),
          mcmcdb_add_parameters.McmcdbWide.matrix)

