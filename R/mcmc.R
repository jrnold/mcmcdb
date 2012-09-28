setClass("Mcmc4", contains="matrix",
         representation(mcpar="numeric"))
mcmc_validity <- function(object) {
    msg <- c()

    if (!is.numeric(object)) {
        msg <- append(msg, "Matrix is not numeric")
    }
    start <- object@mcpar[1]
    end <- object@mcpar[2]
    thin <- object@mcpar[3]
    nobs <- floor((end - start)/thin + 1)
    if (length(thin) < 1) {
        msg <- append(msg, "thin < 1")
    }
    if (nobs < nrow(object)) {
        msg <- append(msg, "Start, end and thin are incompatible with data")
    }
    ## Return
    if (length(msg)) {
        msg
    } else {
        TRUE
    }
}
setValidity("Mcmc4", mcmc_validity)

##' Markov Chain Monte Carlo Objects
##'
##' S4 class which wraps the \bold{coda} S3 class \code{\link[coda]{mcmc}}.
##'
##' @section Slots:
##'
##' \describe{
##'     \item{\code{.Data}:}{Object of class \code{"matrix"}. Parameter matrix.}
##'     \item{\code{mcpar}:}{Object of class \code{"numeric"} of length 3: start, end, and
##'           thin of the iterations. }
##'     \item{\code{.S3Class}:}{This class extends an S3 class.}
##' }
##'
##' @section Extends:
##'
##' Class \code{"\linkS4class{oldClass}"}, directly.
##' Class \code{"\linkS4class{matrix}"}, from data part.
##'
##' @name mcmc-class
##' @rdname mcmc-class
##' @aliases mcmc-class
##' @docType class
##' @seealso \code{\link[coda]{mcmc}}
##' @exportClass mcmc
setOldClass("mcmc", S4Class="Mcmc4")
removeClass("Mcmc4")

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


