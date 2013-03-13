## Methods to get chains / iterations etc

##' Methods to get MCMC object parameter names
##'
##' @param object MCMC object
##'
##' @name mcmcGetParname-method
##' @docType methods
##' @keywords methods
##' @aliases mcmcGetParnames,McmcLong-method
##' @export
setGeneric("mcmcGetParnames",
           function(object, ...) {
               standardGeneric("mcmcGetParnames")
           })

setMethod("mcmcGetParnames", "McmcLong",
          function(object) names(foo@parameters@parameters))

# ----------------------------------

##' Methods to get MCMC object chain names
##'
##' @param object MCMC object
##'
##' @name mcmcGetChains-method
##' @rdname mcmcGetChains-method
##' @docType methods
##' @keywords methods
##' @export
setGeneric("mcmcGetChains",
           function(object, ...) {
               standardGeneric("mcmcGetChains")
           })

setMethod("mcmcGetChains", "McmcLong",
          function(object) unique(object@samples$chain))

# ---------------------------------

##' Methods to get MCMC object chain names
##'
##' @param object MCMC object
##'
##' @rdname mcmcGetIters-method
##' @name mcmcGetIters-method
##' @docType methods
##' @keywords methods
##' @aliases mcmcGetIters,McmcLong-method
##' @export
setGeneric("mcmcGetIters",
           function(object, ...) {
               standardGeneric("mcmcGetIters")
           })

setMethod("mcmcGetIters", "McmcLong",
          function(object) {
              strip_plyr_attr(dlply(object@samples, "chain",
                                    function(x) unique(x$iteration)))
          })
