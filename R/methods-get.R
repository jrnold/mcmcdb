## Methods to get chains / iterations etc

##' Methods to get MCMC object parameter names
##'
##' @param object MCMC object
##'
##' @name mcmcGetParname-method
##' @docType methods
##' @keywords methods
##' @aliases mcmcGetParnames,McmcLong-method
##' @aliases mcmcGetParnames,mcmc.list-method
##' @export
setGeneric("mcmcGetParnames",
           function(object, ...) {
               standardGeneric("mcmcGetParnames")
           })

setMethod("mcmcGetParnames", "mcmc.list",
          function(object) colnames(object[[1]]))

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

setMethod("mcmcGetChains", "mcmc.list",
          function(object) length(object))

# ---------------------------------

##' Methods to get MCMC object chain names
##'
##' @param object MCMC object
##'
##' @rdname mcmcGetIters-method
##' @name mcmcGetIters-method
##' @docType methods
##' @keywords methods
##' @aliases mcmcGetIters,mcmc.list-method
##' @aliases mcmcGetIters,McmcLong-method
##' @export
setGeneric("mcmcGetIters",
           function(object, ...) {
               standardGeneric("mcmcGetIters")
           })

setMethod("mcmcGetIters", "mcmc.list",
          function(object) {
              lapply(object, function(x) seq_len(nrow(x)))
          })
