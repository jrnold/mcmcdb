##' Methods for function \code{rbind2} for MCMC samples
##'
##' @section Methods:
##'
##' \describe{
##' \item{\code{signature(x="mcmc.list",y="missing")}}{
##' Binds all \code{mcmc} objects in a \code{mcmc.list} into a single
##' matrix. This returns a \code{matrix} instead of \code{mcmc} since
##' the start/end/thin values would no longer make sense.}
##' }
##'
##' @rdname rbind2-methods
##' @name rbind2-methods
##' @aliases rbind2,mcmc.list,missing-method
##' @export
setMethod("rbind2", signature(x="mcmc.list", y="missing"),
         function(x, y, ...) {
             do.call(rbind, x)
         })
