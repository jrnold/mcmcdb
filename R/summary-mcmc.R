##' @exportClass summary.mcmc
setOldClass("summary.mcmc")

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


