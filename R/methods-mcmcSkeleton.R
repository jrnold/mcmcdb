##' @exportMethod mcmcSkeleton
NULL

##' Create skeleton for relisting MCMC samples
##'
##' @name mcmcSkeleton
##' @rdname mcmcSkeleton
##' @param x \code{McmcPararrays} object
##' @return \code{list} of numeric \code{array} objects, one for
##' each value of \code{pararray}, and in the dimension
##' of that \code{parrray}, but with all entries set to 0.
##' @docType methods
##' @keywords methods
##' @aliases mcmcSkeleton-methods
##' @aliases mcmcSkeleton,McmcPararrays-method
##' @aliases mcmcSkeleton,data.frame-method
##' @aliases mcmcSkeleton,McmcLong-method
NULL

setGeneric("mcmcSkeleton",
           function(x, ...) standardGeneric("mcmcSkeleton"))

mcmc_pararrays_to_skeleton <- function(x) {
    result <-
        dlply(x, "pararray",
              function(x) {
                  zeros(dim=eval(parse(text=sprintf("c(%s)",
                                       x$dim_sz))))
              })
    strip_plyr_attr(result)
}

setMethod("mcmcSkeleton", "McmcPararrays", mcmc_pararrays_to_skeleton)

setMethod("mcmcSkeleton", "data.frame",
          function(x) callGeneric(McmcPararray(x)))

setMethod("mcmcSkeleton", "McmcLong",
          function(x) callGeneric(x@pararrays))
