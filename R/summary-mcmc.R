##' MCMC summary class
##'
##' @section Slots
##'
##' \describe{
##' \item{\code{.Data}}{\code{list} with elements
##' "statistics", "quantiles", "start", "end", "thin", and "nchain".}
##' }
##'
##' @section Extends
##'
##' \describe{
##' \item{list}{directly}
##' \item{oldClass}{directly}
##' }
##'
##' @name summary.mcmc-class
##' @rdname summary.mcmc-class
##' @exportClass summary.mcmc
setClass("SummaryMcmc4", "list")
validity_summary_mcmc <- function(object) {
    msg <- c()
    if (names(object) != c("statistics", "quantiles", "start",
             "end", "thin", "nchain")) {
        msg <- append("Incorrect element names")
    }
    ## TODO: check individual elements
    if (length(msg)) {
        msg
    } else {
        TRUE
    }
}
setValidity("SummaryMcmc4", validity_summary_mcmc)
setOldClass("summary.mcmc", S4="SummaryMcmc4")
removeClass("SummaryMcmc4")

## summary.mcmc methods
##' @export
setAs("summary.mcmc", "data.frame",
      function(from) {
          to <- data.frame(as(from, "matrix"))
          to$variable <- rownames(to)
          to
      })

##' @export
setAs("summary.mcmc", "matrix",
      function(from) {
          cbind(from$statistics, from$quantiles)
      })



