
setClass("McmcList4", contains="list")
mcmc_list_validity <- function(object) {
    ## Allow for empty lists
    if (length(object@.Data) == 0) {
        TRUE
    } else {
        allmcmc <- all(sapply(object, is, class2="mcmc"))
        if (allmcmc) {
            TRUE
        } else {
            "Not all elements in the list are mcmc objects."
        }
    }
}
setValidity("McmcList4", mcmc_list_validity)

##' Markov Chain Monte Carlo Object List
##'
##' S4 class which wraps the \bold{coda} S3 class \code{\link[coda]{mcmc.list}}.
##' This is simply a list of \code{\link{mcmc}} object.
##'
##' @section Slots:
##'
##' \describe{
##'     \item{\code{.Data}:}{Object of class \code{"list"} of \code{"mcmc"} objects. }
##'     \item{\code{.S3Class}:}{Object of class \code{"character"}, name of S3 class this extends. }
##' }
##'
##' @section Extends:
##'
##' Class \code{"\linkS4class{list}"}, from data part.
##'
##' @name mcmc.list-class
##' @rdname mcmc.list-class
##' @aliases mcmc.list-class
##' @docType class
##' @seealso \code{\link[coda]{mcmc}}
##' @exportClass mcmc.list
setOldClass("mcmc.list", S4Class="McmcList4")
removeClass("McmcList4")

##' @export
mcmc.list <- function (x, ...) {
    new("mcmc.list", c(x, list(...)))
}
##' Create mcmc.list
##'
##' Unlike \code{\link[coda]{mcmc.list}} this does
##' not enforce equal
##'
##' @param ... \code{mcmc} objects.
##' @export
setGeneric("mcmc.list")

## Apply function to Mmcm Scalar Parameter over ALL chains
## Use one column at a time to conserve memory
mcmc_iter_column <- function(x, FUN=identity, ...) {
    n <- ncol(x[[1]])
    sapply(seq_len(n),
           function(i){
               FUN(unlist(lapply(x, function(j) as.numeric(j[ , i]))), ...)
           })
}

##' @export
setMethod("mean", "mcmc.list",
          function(x, ...) {
              mcmc_iter_column(x, mean, ...)
          })

##' @export
setMethod("median", "mcmc.list",
          function(x, na.rm=FALSE) {
              mcmc_iter_column(x, median, na.rm=na.rm)
          })

##' @export
setMethod("quantile", "mcmc.list",
          function(x, ...) {
              mcmc_iter_column(x, quantile, ...)
          })


## mcmc.list methods
##' @export
setMethod("coef", "mcmc.list",
          function(object, FUN="mean", ...) {
              apply(do.call(rbind, object), 2, FUN)
          })

##' @export
setMethod("vcov", "mcmc.list",
          function(object, ...) {
              cov(do.call(rbind, object), ...)
          })


