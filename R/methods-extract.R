## Extract Methods for Mcmc Objects
## Extract methods
## [parname, chainid, iter, value? ]
## [[parameter]], $parameter

## setClassUnion("McmcIndex1", c("character", "missing", "function", "logical"))
## setClassUnion("McmcIndex2", c("integer", "missing", "function", "logical"))

stopifnotbool <- function(x) {
    stopifnot(is(x, "logical"))
}

extract1_mcmc_long <- function(x, i=TRUE, j=TRUE, k=TRUE, val=TRUE, pararray=FALSE) {
    if (!missing(i)) {
        if (pararray) {
            # print("using pararray")
            if (is.character(i)) {
                ii <- x@parnames[["pararray"]] == i
            } else if (is(i, "numeric")) {
                lvl <- levels(x@parnames[["pararray"]])[i]
                ii <- x@parnames[["pararray"]] == lvl
            } else if (is(i, "function")) {
                ii <- i(x@parnames[["pararray"]])
            } else {
                ii <- as.logical(i)
            }
            i <- as.character(x@parnames[ii, "parname"])
            # print(i)
        }
        if (is.character(i)) {
            i <- x@samples[["parname"]] == i
        } else if (is(i, "numeric")) {
            lvl <- levels(x@samples[["parname"]])[i]
            i <- x@samples[["parname"]] == lvl
        } else if (is(i, "function")) {
            i <- i(x@samples[["parname"]])
        } else {
            i <- as.logical(i)
        }
    } else {
        i <- TRUE
    }
    stopifnotbool(i)
    # Chainid
    if (!missing(j)) {
        if (is(j, "numeric")) {
            j <- x@samples[["chain_id"]] == as(j, "integer")
        } else if (is(j, "function")) {
            j <- j(x@samples[["chain_id"]])
        } else {
            j <- as.logical(j)
        }
    } else {
        j <- TRUE
    }
    stopifnotbool(j)
    if (!missing(k)) {
        # Iter
        if (is(k, "numeric")) {
            k <- x@samples[["iter"]] == as(k, "integer")
        } else if (is(k, "function")) {
            k <- k(x@samples[["iter"]])
        } else {
            k <- as.logical(k)
        }
    } else {
        k <- TRUE
    }
    stopifnotbool(k)
    ## Val constraints
    if (!missing(val)) {
        if (is(val, "function")) {
            val <- val(x@samples[["val"]])
        } else if (is(val, "numeric")) {
            val <- x@samples[["val"]] == val
        } else {
            val <- as.logical(val)
        }
    } else {
        val <- TRUE
    }
    stopifnotbool(val)
    # Ex@samplestract
    x@samples[i & j & k & val, ]
}

##' @exportMethod [
setMethod("[", signature(x="McmcLong",
                         i="ANY",
                         j="ANY"),
          extract1_mcmc_long)

##' @exportMethod [[
setMethod("[[", signature(x="McmcLong", i="character"),
          function(x, i) x[x@samples[["parname"]] == i, ])

##' @exportMethod $
setMethod("$", signature(x="McmcLong"),
          function(x, name) x[[name]])
