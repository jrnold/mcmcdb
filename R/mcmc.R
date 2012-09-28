setClass("Mcmc4", contains="matrix",
         representation(mcpar="numeric"))
mcmc_validity <- function(object) {
    msg <- c()
    start <- object@mcpar[1]
    end <- object@mcpar[2]
    thin <- object@mcpar[3]
    if (((end - start + 1) / thin) != nrow(object@.Data)) {
        msg <-
            append(msg,
                   sprintf("start=%d,end=%d,thin=%d and are incompatible with the data (nrow=%d)",
                           start, end, thin, nrow(object@.Data)))
    }
    if (!all(round(object@mcpar) == object@mcpar)) {
        msg <- append(msg,
                      sprintf("start=%d,end=%d,thin=%d are not integers",
                              start, end, thin))
    }
    if (!is.numeric(object@.Data)) {
        msg <- append(msg, "matrix is not numeric")
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

##' Initialize
initialize_mcmc <- function(.Object, data,
                            start=1,
                            end=start + nrow(data) - 1,
                            thin=(end - start + 1)/nrow(data))
{
    .Object@.Data <- data
    .Object@mcpar <- c(start[1], end[1], thin[1])
    validObject(.Object)
    .Object
}
setMethod("initialize", "mcmc", initialize_mcmc)

##' Create mcmc
##'
##' Generic function to create mcmc objects from various types of inputs.
##'
##' @param data Input object
##' @export
setGeneric("mcmc",
           function(data, ...) {
               new("mcmc", data, ...)
           })

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
    callGeneric(matrix(data), start=start, end=end, thin=thin)
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


