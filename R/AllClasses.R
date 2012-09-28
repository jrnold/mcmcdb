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


##' Summary Markov Chain Monte Carlo Objects
##'
##' S4 class which wraps the \bold{coda} S3 class \code{\link[coda]{summary.mcmc}}.
##'
##' @name summary.mcmc-class
##' @rdname summary.mcmc-class
##' @aliases summary.mcmc-class
##' @docType class
##' @seealso \code{\link[coda]{summary.mcmc}}
##' @exportClass summary.mcmc
setOldClass("summary.mcmc", S4Class="SummaryMcmc4")
removeClass("SummaryMcmc4")
