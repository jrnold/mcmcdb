##' @exportClass mcmc.list
NULL

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
NULL

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
setOldClass("mcmc.list", S4Class="McmcList4")
removeClass("McmcList4")

mcmc.list <- function (x, ...) {
    if (!all(sapply(x, is, class2="mcmc"))) {
        x <- lapply(x, mcmc, ...)
    }
    new("mcmc.list", x)
}
##' Create mcmc.list
##'
##' Unlike \code{\link[coda]{mcmc.list}} this does
##' not enforce equal
##'
##' @param ... \code{mcmc} objects.
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
##
mcmc_iter_column <- function(x, FUN=identity, ...) {
    n <- ncol(x[[1]])
    .FUN <- match.fun(FUN)
    sapply(seq_len(n),
           function(i){
               .FUN(unlist(lapply(x, function(j) as.numeric(j[ , i]))), ...)
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
              mcmc_iter_column(object, FUN=FUN, ...)
          })

##' @export
setMethod("vcov", "mcmc.list",
          function(object, ...) {
              cov(as(object, "matrix"), ...)
          })

##' Mcmc.list into a matrix.
##'
##' Binds all \code{mcmc} objects in a \code{mcmc.list} into a single
##' matrix. This returns a \code{matrix} instead of \code{mcmc} since
##' the start/end/thin values would no longer make sense. However,
##' \code{as(from, "mcmc")} will transfrom an \code{mcmc.list} object
##' into a \code{mcmc} object with all the chains stacked on top of
##' each other.
##'
##' @return \code{matrix} object with columns equal to the number of
##' parameters in the \code{mcmc} objects, and a number of rows equal
##' to the sum of the iterations over all chains.
##'
##' @export
setMethod("rbind2", signature(x="mcmc.list", y="missing"),
         function(x, y, ...) {
             do.call(rbind, x)
         })

##' @export
setMethod("melt", "mcmc.list",
          function(data, ...) {
              chains <- length(data)
              data_len <- laply(data, function(x) prod(dim(x)))
              result <- do.call(rbind, llply(data, melt))
              result$chain <-
                  as.integer((mapply(function(x, y) rep(x, y),
                                     seq_len(chains), data_len)))
              rownames(result) <- with(result, paste(parameter,
                                                     iteration, chain, sep="."))
              result
          })

## Coercion
## From mcmc.list
setAs("mcmc.list", "matrix", function(from) rbind2(from))
setAs("mcmc.list", "mcmc", function(from) mcmc(as(from, "matrix")))

## To mcmc.list
setAs("mcmc", "mcmc.list",
      function(from) {
          new("mcmc.list", list(from))
      })

