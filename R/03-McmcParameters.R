
##' MCMC Parameter Metadata
##'
##' This class stores the mapping between the names of MCMC parameters
##' in their flat representation (as returned by samplers like BUGS)
##' and their array representation in the model. This class
##' facilitates transforming parameters from their flat to their array
##' representation.
##'
##' @section Slots:
##'
##' \describe{
##'
##' \item{\code{parameters}:}{Object of class \code{"character"}.
##' Values are the names of the array parameters; names are the names
##' of the flat parameters. }
##'
##' \item{\code{skeleton}:}{Object of class \code{"list"} List of
##' arrays. Each element has the name of the array parameter, and an
##' array value of the proper shape of that parameter.}
##'
##' \item{\code{indices}:}{Object of class \code{"list"} List of
##' matrices, one for each array parameter. Each row the matrix is a
##' has a rowname of a flat parameter and values that are the index of
##' flat parameter in the array parameter.}
##' }
##'
##' @rdname McmcParameters-class
##' @aliases McmcParameters-class
##' @docType class
##' @keywords classes
##' @export
setClass("McmcParameters",
         representation(parameters="character",
                        skeleton="list",
                        indices="list"))

validate_mcmc_parameter_meta <- function(object) {
    par_flat <- names(object@parameters)
    par_array <- unique(object@parameters)
    tmpl_dim <- lapply(object@skeleton, dim)
    indices_max <- lapply(object@indices, function(x) apply(x, 2, max))
    if (!setequal(par_array, names(object@skeleton))) {
        return("names(object@skeleton) disagree with the names in object@parameters")
    }
    if (!setequal(par_array, names(object@indices))) {
        return("names(object@indices) disagree with the names in object@parameters")
    }
    if (!setequal(unlist(sapply(object@indices, rownames)), par_flat)) {
        return("rownames in object@indices disagree with the parameters in object@parameters")
    }
    if (any(mapply(function(x, y) any(x > y), indices_max, tmpl_dim))) {
        return("an index in object@indices is out of range of objects@skeleton")
    }
    if (any(unlist(sapply(object@indices, `<`, y=1)))) {
        return("values in object@indices cannot be less than 1")
    }
    TRUE
}

setValidity("McmcParameters", validate_mcmc_parameter_meta)


##' Create \code{McmcParameters} objects
##'
##' @rdname McmcParameters-methods
##' @name McmcParameters-methods
##' @keywords methods
##' @aliases McmcParameters,data.frame-method
##' @aliases McmcParameters,matrix-method
##' @aliases McmcParameters,character-method
##' @aliases McmcParameters,factor-method
##' @docType methods
##' @keywords methods
##' @export
setGeneric("McmcParameters", function(x, ...) standardGeneric("McmcParameters"))

mcmc_parameter_meta_data_frame <- function(x, ...) {
    new("McmcParameters",
        parameters=parsed_parameters_to_parameters(x),
        skeleton=parsed_parameters_to_skeleton(x),
        indices=parsed_parameters_to_indices(x))
}


setMethod("McmcParameters", "data.frame", mcmc_parameter_meta_data_frame)

setMethod("McmcParameters", "matrix",
          function(x, ...) {
              callGeneric(as(x, "data.frame"), ...)
          })

setMethod("McmcParameters", "character",
          function(x, fun=mcmc_parse_parname_default, ...) {
              callGeneric(fun(x, ...))
          })

setMethod("McmcParameters", "factor",
          function(x, fun=mcmc_parse_parname_default, ...) {
              callGeneric(as.character(x), fun=fun, ...)
          })
