##' @exportClass summary.mcmc
setClass("SummaryMcmcS4", "list")
setValidity(function(object) {
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
})
setOldClass("summary.mcmc", S4="SummaryMcmcS4")
removeClass("SummaryMcmcS4")

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



