##' @exportClass McmcList2
##' @exportMethod McmcList2
NULL

##' List of MCMC chains with parameter metadata
##'
##' This class extends the standard \code{\link[coda]{mcmc.list}}
##' class with metadata about the shape of the parameters that can be
##' used to convert them back to their original shapes to make
##' postprocessing easier.
##'
##' @section Slots:
##'
##' \describe{
##' \item{\code{parameters}}{Class \code{McmcParamterMeta}.}
##' }
##'
##' @section Extends:
##'
##' \describe{
##' \item{\code{mcmc.list}}{directly}
##' }
##'
##' mcmc class with parameter metadata
##'
##' @section Extends:
##'
##' \describe{
##' \item{\code{parameters}}{\code{McmcParameterMeta} object}
##' }
##'
##' @rdname McmcList2-class
##' @aliases McmcList2-class
##' @docType class
##' @keywords classes
setClass("McmcList2", contains="mcmc.list",
         representation(parameters="McmcParameterMeta"))


##' Create \code{McmcList2} objects
##'
##' @section Methods:
##' \describe{
##' \item{\code{signature(data="McmcList2")}}{Create from \code{mcmc.list}.}
##' \item{\code{signature(data="McmcList2")}}{Create from \code{matrix}.}
##' }
##'
##' @rdname McmcList2-methods
##' @name McmcList2-methods
##' @keywords method
##' @aliases McmcList2-methods
##' @aliases McmcList2,matrix-method
##' @aliases McmcList2,mcmc.list-method
##' @docType methods
##' @keywords methods
##' @export
setGeneric("McmcList2",
           function(data, ...) {
               standardGeneric("McmcList2")
           })


mcmc2_default <- function(data, ...,
                          parameter_names=colnames(data[[1]]),
                          fun=parse_parameter_names_default) {
    ## Put this before parparsed to change data before eval
    new("McmcList2", mcmc.list(data),
        parameters=McmcParameterMeta(fun(parameter_names)))
}

setMethod("McmcList2", "mcmc.list", mcmc2_default)

setMethod("McmcList2", "matrix",
          function(data, ...) callGeneric(mcmc.list(data), ...))

