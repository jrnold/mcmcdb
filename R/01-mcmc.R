##' @exportClass mcmc
NULL

##' Markov Chain Monte Carlo Objects
##'
##' S4 class which wraps the \pkg{coda} S3 class \code{\link[coda]{mcmc}}.
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

##' Create \code{mcmc} objects
##'
##' Create an \code{mcmc} object.
##'
##' @section Methods:
##' \describe{
##' \item{\code{signature(data = "ANY")}}{}
##' \item{\code{signature(data = "numeric")}}{}
##' \item{\code{signature(data = "ts")}}{}
##' }
##'
##' @aliases mcmc mcmc-method
##' @aliases mcmc,ANY-method
##' @aliases mcmc,numeric-method
##' @aliases mcmc,ts-method
##' @param data Input object
##' @rdname mcmc-methods
##' @name mcmc-methods
##' @docType methods
##' @keywords methods
NULL

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

setMethod("mcmc", "ts", mcmc_ts)

