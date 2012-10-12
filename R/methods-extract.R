##' Extract Methods for Mcmc Objects
## Extract methods
## [parameter, chain, iteration, value? ]
## [[parameter]], $parameter

## setClassUnion("McmcIndex1", c("character", "missing", "function", "logical"))
## setClassUnion("McmcIndex2", c("integer", "missing", "function", "logical"))

stopifnotbool <- function(x) {
    stopifnot(is(x, "logical"))
}

extract1_mcmc_long <- function(x, i=TRUE, j=TRUE, k=TRUE, value=TRUE) {
    if (!missing(i)) {
        if (is.character(i)) {
            i <- x@samples[["parameter"]] == i
        } else if (is(i, "numeric")) {
            lvl <- levels(x@samples[["parameter"]])[i]
            i <- x@samples[["parameter"]] == lvl
        } else if (is(i, "function")) { 
            i <- i(x@samples[["parameter"]])
        } else {
            i <- as.logical(i)
        }
    } else {
        i <- TRUE
    }
    stopifnotbool(i)
    # Chain
    if (!missing(j)) {
        if (is(j, "numeric")) {
            j <- x@samples[["chain"]] == as(j, "integer")
        } else if (is(j, "function")) {
            j <- j(x@samples[["chain"]])
        } else {
            j <- as.logical(j)
        }
    } else {
        j <- TRUE
    }
    stopifnotbool(j)
    if (!missing(k)) {
        # Iteration
        if (is(k, "numeric")) {
            k <- x@samples[["iteration"]] == as(k, "integer")
        } else if (is(k, "function")) { 
            k <- k(x@samples[["iteration"]])
        } else {
            k <- as.logical(k)
        }
    } else {
        k <- TRUE
    }
    stopifnotbool(k)
    ## Value constraints
    if (!missing(value)) {
        if (is(value, "function")) {
            value <- value(x@samples[["value"]])
        } else if (is(value, "numeric")) {
            value <- x@samples[["value"]] == value
        } else {
            value <- as.logical(value)
        }
    } else {
        value <- TRUE
    }
    stopifnotbool(value)
    # Ex@samplestract
    x@samples[i & j & k & value, ]
}

##' @exportMethod [
setMethod("[", signature(x="McmcLong",
                         i="ANY",
                         j="ANY"),
          extract1_mcmc_long)

##' @exportMethod [[
setMethod("[[", signature(x="McmcLong", i="character"),
          function(x, i) x[x@samples[["parameter"]] == i, ])

##' @exportMethod $
setMethod("$", signature(x="McmcLong"),
          function(x, name) x[[name]])


