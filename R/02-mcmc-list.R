##' @exportClass mcmc.list
NULL

##' MCMC object list
##'
##' S4 class which wraps the \pkg{coda} S3 class
##' \code{\link[coda]{mcmc.list}}. This is simply a list of
##' \code{\link{mcmc}} object.
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
##' @keywords classes
##' @seealso \code{\link[coda]{mcmc}}
NULL

setClass("McmcList4", contains="list")
mcmc_list_validity <- function(object) {
    ## Allow for empty lists
    if (length(object@.Data) == 0) {
        TRUE
    } else {
        allmcmc <- all(sapply(object, is, class="mcmc"))
        if (allmcmc) {
            TRUE
        } else {
            "Not all elements in the list are mcmc objects."
        }
    }
}
setValidity("McmcList4", mcmc_list_validity)
setOldClass("mcmc.list", S4Class="McmcList4")
removeClass("McmcList4")


##' Create \code{mcmc.list} object
##'
##' @section Details:
##'
##' Unlike \code{\link[coda]{mcmc.list}} this does
##' not enforce equal sample sizes.
##'
##' @param x Object with MCMC samples.
##' @param ... Arguments passed to \code{\link{mcmc}}.
##'
##' @rdname mcmc.list-methods
##' @name mcmc.list-methods
##'
##' @aliases mcmc.list mcmc.list,ANY-method mcmc.list,mcmc-method mcmc.list,matrix-method
##' @seealso \code{\link[coda]{mcmc}}
##' @docType methods
##' @keywords methods
NULL

mcmc.list <- function (x, ...) {
    if (!all(sapply(x, is, class="mcmc"))) {
        x <- lapply(x, mcmc, ...)
    }
    new("mcmc.list", x)
}

##' @export
setGeneric("mcmc.list")

setMethod("mcmc.list", "mcmc",
          function(x, ...) {
              callGeneric(list(x), ...)
          })

setMethod("mcmc.list", "matrix",
          function(x, ...) {
              callGeneric(mcmc(x), ...)
          })

## Apply function to parameters concatenated over all chains
##
## This iterates one column at a time to conserve memory (I hope).
##
mcmc_iter_column <- function(x, FUN=identity, ...) {
    n <- ncol(x[[1]])
    parnames <- colnames(x[[1]])
    .FUN <- match.fun(FUN)
    ret <- sapply(seq_len(n),
                  function(i){
                      .FUN(unlist(lapply(x, function(j) as.numeric(j[ , i]))), ...)
                  })
    if (length(dim(ret)) > 0) {
        colnames(ret) <- parnames
    } else {
        names(ret) <- parnames
    }
    ret
}

## Coercion
## From mcmc.list
setAs("mcmc.list", "matrix", function(from) rbind2(from))
setAs("mcmc.list", "mcmc", function(from) mcmc(as(from, "matrix")))

## To mcmc.list
setAs("mcmc", "mcmc.list",
      function(from) {
          new("mcmc.list", list(from))
      })

