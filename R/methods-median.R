##' Methods for function \code{methods} for MCMC samples
##'
##' Calculate the median for each parameter of an MCMC sample object.
##'
##' @section Methods:
##'
##' \describe{
##' \item{\code{signature(x="mcmc")}}{Method for class \code{mcmc}.}
##' \item{\code{signature(x="median")}}{Method for class \code{mcmc.list}.}
##' }
##'
##' @rdname median-methods
##' @name median-methods
##' @aliases median,mcmc-method median,mcmc.list-method
##' @seealso \code{\link{median}}
NULL

##' @export
setGeneric("median", stats::median)

##' @aliases median,mcmc-method
setMethod("median", "mcmc",
          function(x, na.rm=FALSE) {
              apply(x, 2, median, na.rm=na.rm)
          })


setMethod("median", "mcmc.list",
          function(x, na.rm=FALSE) {
              mcmc_iter_column(x, median, na.rm=na.rm)
          })
