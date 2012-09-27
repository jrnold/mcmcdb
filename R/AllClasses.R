##' @exportClass mcmc
##' @exportClass mcmc.list
##' @exportClass summary.mcmc
NULL

##' Markov Chain Monte Carlo Objects
##'
##' S4 class which wraps the \bold{coda} S3 class \code{\link[coda]{mcmc}}.
##'
##' \section{Slots}{
##'   \describe{
##'     \item{\code{.Data}:}{Object of class \code{"matrix"}. Parameter matrix.}
##'     \item{\code{mcpar}:}{Object of class \code{"numeric"} of length 3: start, end, and
##'           thin of the iterations. }
##'     \item{\code{.S3Class}:}{This class extends an S3 class.}
##'   }
##' }
##' \section{Extends}{
##'   Class \code{"\linkS4class{oldClass}"}, directly.
##'   Class \code{"\linkS4class{matrix}"}, from data part.
##' }
##' @name mcmc-class
##' @rdname mcmc-class
##' @aliases mcmc-class
##' @docType class
##' @seealso \code{\link[coda]{mcmc}}
setClass("Mcmc4", contains="matrix",
         representation(mcpar="numeric"))
setOldClass("mcmc", S4Class="Mcmc4")

##' Markov Chain Monte Carlo Object List
##'
##' S4 class which wraps the \bold{coda} S3 class \code{\link[coda]{mcmc.list}}.
##' This is simply a list of \code{\link{mcmc}} object.
##'
##' \section{Slots}{
##'   \describe{
##'     \item{\code{.Data}:}{Object of class \code{"list"} of \code{"mcmc"} objects. }
##'     \item{\code{.S3Class}:}{Object of class \code{"character"}, name of S3 class this extends. }
##'   }
##' }
##' \section{Extends}{
##'   Class \code{"\linkS4class{list}"}, from data part.
##' }
##' @name mcmc.list-class
##' @rdname mcmc.list-class
##' @aliases mcmc.list-class
##' @docType class
##' @seealso \code{\link[coda]{mcmc}}
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
## removeClass("McmcList4")


##' Summary Markov Chain Monte Carlo Objects
##'
##' S4 class which wraps the \bold{coda} S3 class \code{\link[coda]{summary.mcmc}}.
##'
##'
##'
##' @seealso \code{\link[coda]{summary.mcmc}}
setClass("SummaryMcmc4",
         representation(statistics = "matrix",
                        quantiles = "matrix",
                        start = "numeric",
                        end = "numeric",
                        thin = "numeric",
                        nchain = "numeric"))
setOldClass("summary.mcmc", S4Class="SummaryMcmc4")
## removeClass("SummaryMcmc4")
