##' Create mcmc
##'
##' Generic function to create mcmc objects from various types of inputs.
##'
##' @param data Input object
##' @export
setGeneric("mcmc",
           function(data, ...) {
               standardGeneric("mcmc")
           })
mcmc_matrix <- function(data, start=1, end=NULL, thin=1, ...) {
    niter <- nrow(data)
    if (missing(end))
        end <- start + (niter - 1) * thin
    else if (missing(start))
        start <- end - (niter - 1) * thin
    nobs <- floor((end - start)/thin + 1)
    if (niter < nobs)
        stop("Start, end and thin incompatible with data")
    else {
        end <- start + thin * (nobs - 1)
        if (nobs < niter)
            data <- data[1:nobs, , drop = FALSE]
    }
    new("mcmc", data, mcpar=c(start, end, thin))
}

##' @param data start Iteration number for end of sample.
##' @param data end Iteration number for start of sample.
##' @param thin end Iteration number for start of sample.
##' @rdname mcmc
setMethod("mcmc", "matrix", mcmc_matrix)

##' @param ... passed to next generic
##' @rdname mcmc
mcmc_numeric <- function(data, ...) {
    callGeneric(as.matrix(data), ...)
}
setMethod("mcmc", "numeric", mcmc_numeric)

##' @rdname mcmc
mcmc_ts <- function(data, ...) {
    start <- start(data)[1]
    end <- end(data)[1]
    thin <- deltat(data)
    ## Make sure that thin is at least 1
    if (thin < 1) {
        end <- start + (end - start) / thin
        thin <- 1
    }
    callGeneric(as.matrix(data),
                start=start, end=end, thin=thin)
}
setMethod("mcmc", "ts", mcmc_ts)

## mcmc  methods
##' @export
setMethod("coef", "mcmc",
          function(object, FUN="mean", ...) {
              apply(object, 2, FUN)
          })
##' @export
setMethod("vcov", "mcmc",
          function(object, ...) cov(object, ...))

##' Mcmc Generic Functions
##' @export
setMethod("mean", "mcmc",
          function(x, ...) {
              apply(x, 2, mean, ...)
          })
##' @export
setMethod("quantile", "mcmc",
          function(x, ...) {
              apply(x, 2, quantile, ...)
          })
##' @export
setMethod("median", "mcmc",
          function(x, na.rm=FALSE) {
              apply(x, 2, median, na.rm=na.rm)
          })


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


