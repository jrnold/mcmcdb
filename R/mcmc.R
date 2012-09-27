Mcmc <- function(x, start=1, end=nrow(x), thin=1) {
    new("mcmc", as.numeric(x),
        mcpar=c(start=start, end=end, thin=thin))
}

## mcmc  methods
setMethod("coef", "mcmc",
          function(object, FUN="mean", ...) {
              apply(object, 2, FUN)
          })
setMethod("vcov", "mcmc",
          function(object, ...) cov(object, ...))

##' Mcmc Generic Functions
setMethod("mean", "mcmc",
          function(x, ...) {
              apply(x, 2, mean, ...)
          })
setMethod("quantile", "mcmc",
          function(x, ...) {
              apply(x, 2, quantile, ...)
          })
setMethod("median", "mcmc",
          function(x, na.rm=FALSE) {
              apply(x, 2, median, na.rm=na.rm)
          })


## summary.mcmc methods
setAs("summary.mcmc", "data.frame",
      function(from) {
          to <- data.frame(cbind(from@statistics, from@quantiles))
          to$variable <- rownames(to)
          to
      })


## Apply function to Mmcm Scalar Parameter over ALL chains
## Use one column at a time to conserve memory
mcmc_iter_column <- function(x, FUN=identity, ...) {
    n <- ncol(x[[1]])
    sapply(seq_len(n),
           function(i){
               FUN(unlist(lapply(x, function(j) as.numeric(j[ , i]))), ...)
           })
}
setMethod("mean", "mcmc.list",
          function(x, ...) {
              mcmc_iter_column(x, mean, ...)
          })
setMethod("median", "mcmc.list",
          function(x, na.rm=FALSE) {
              mcmc_iter_column(x, median, na.rm=na.rm)
          })
setMethod("quantile", "mcmc.list",
          function(x, ...) {
              mcmc_iter_column(x, quantile, ...)
          })


## mcmc.list methods
setMethod("coef", "mcmc.list",
          function(object, FUN="mean", ...) {
              apply(do.call(rbind, object), 2, FUN)
          })
setMethod("vcov", "mcmc.list",
          function(object, ...) {
              cov(do.call(rbind, object), ...)
          })

