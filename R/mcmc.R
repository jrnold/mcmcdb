##' @exportClass mcmc
NULL

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
NULL

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
setOldClass("mcmc", S4Class="Mcmc4")
removeClass("Mcmc4")

initialize_mcmc <- function(.Object, data,
                            start=1,
                            end=start + nrow(data) - 1,
                            thin=(end - start + 1)/nrow(data)) {
    .Object@.Data <- data
    .Object@mcpar <- sapply(list(start, end, thin), first)
    validObject(.Object)
    .Object
}
setMethod("initialize", "mcmc", initialize_mcmc)

##' Create mcmc objects
##'
##' Create an \code{mcmc} object.
##'
##' \describe{
##' \item{\code{signature(data = "ANY")}}{}
##' \item{\code{signature(data = "numeric")}}{}
##' \item{\code{signature(data = "ts")}}{}
##' }
##'
##' @aliases mcmc-methods
##' @aliases mcmc,ANY-method
##' @aliases mcmc,numeric-method
##' @aliases mcmc,ts-method
##' @param data Input object
##' @rdname mcmc
##' @export
setGeneric("mcmc",
           function(data, ...) {
               new("mcmc", data, ...)
           })

setMethod("mcmc", "numeric",
          function(data, ...) callGeneric(as.matrix(numeric)))

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

##' @rdname mcmc
setMethod("mcmc", "ts", mcmc_ts)

##' mcmc basic generic functions
##'
##' This method is meant to be equivalent to the "coef" method for
##' "mle" objects, in the sense that it returns a vector point
##' estimate for each parameter. By default, the point estimate
##' returned is the mean.
##'
##' @param FUN Function used to calculate the point estimate.
##'
##' @rdname mcmc-generics
##' @export
##' @aliases vcov,mcmc-method
setMethod("coef", "mcmc",
          function(object, FUN="mean", ...) {
              apply(object, 2, FUN)
          })

##' @rdname mcmc-generics
##' @export
##' @aliases vcov,mcmc-method
setMethod("vcov", "mcmc",
          function(object, ...) cov(object, ...))

##' @rdname mcmc-generics
##' @export
##' @aliases mean,mcmc-method
setMethod("mean", "mcmc",
          function(x, ...) {
              apply(x, 2, mean, ...)
          })

##' @rdname mcmc-generics
##' @export
##' @aliases quantile,mcmc-method
setMethod("quantile", "mcmc",
          function(x, ...) {
              apply(x, 2, quantile, ...)
          })

##' @rdname mcmc-generics
##' @export
##' @aliases median,mcmc-method
setMethod("median", "mcmc",
          function(x, na.rm=FALSE) {
              apply(x, 2, median, na.rm=na.rm)
          })

##' melt mcmc objects into a data.frame
##'
##' This melts an \code{mcmc} object into a data frame
##'
##' @return \code{data.frame} with columns
##' \describe{
##' \item{\code{iteration}}{\code{integer}}
##' \item{\code{parameter}}{\code{factor}}
##' \item{\code{value}}{\code{numeric}}
##' }
##'
##' @export
setMethod("melt", "mcmc",
    function(data, ...) {
        result <- callGeneric(as(data, "matrix"))
        colnames(result)[1:2] <- c("iteration", "parameter")
        rownames(result) <- with(result, paste(parameter, iteration, sep="."))
        result
    })

