#' @include package.R
#' @include class-McmcdbWide.R
NULL

setGeneric("mcmcdb_set_iters<-",
           function(object, ..., value) {
             standardGeneric("mcmcdb_set_iters<-")
           })

`mcmcdb_set_iters.McmcdbWide.NULL<-` <-
  function(object, chain_id = NULL, iter = NULL, test=TRUE, value) {
    touse <- 
      ! mcmcdb_wide_select_iters(object, chain_id = chain_id,
                                 iter = iter)
    object@samples <- object@samples[touse, ]
    object@iters <- object@iters[touse, ]
    if (test) {
      validObject(object)
    }
    object
  }

setReplaceMethod("mcmcdb_set_iters",
                 c(object = "McmcdbWide", value = "NULL"),
                 `mcmcdb_set_iters.McmcdbWide.NULL<-`)


`mcmcdb_set_iters.McmcdbWide.list<-` <-
  function(object, value, test=TRUE) {
    if (! is(value[["iters"]], "McmcdbIters")) {
      stop(sprintf("value[[\"iters\"]] is not an object of class %s",
                   dQuote("McmcdbIters")))
    }
    if (! is(value[["samples"]], "matrix")) {
      stop(sprintf("value[[\"samples\"]] is not an object of class %s",
                   dQuote("matrix")))
    }
    if (!setequal(colnames(value[["samples"]]), colnames(object@samples))) {
      stop("colnames of value[[\"samples\"]] are not equal to the parameters")
    }
    object@samples <- rbind(object@samples,
                            value[["samples"]][ , colnames(object@samples)])
    object@iters <- rbind2(object@iters, value[["iters"]])
    if (test) {
      validObject(object)
    }
    object
  }

setReplaceMethod("mcmcdb_set_iters",
                 c(object = "McmcdbWide", value = "list"),
                 `mcmcdb_set_iters.McmcdbWide.list<-`)

