## Extract methods
## [parameter, chain, iteration, value? ]
## [[parameter]], $parameter

setClassUnion("McmcIndex1", c("character", "missing", "function", "logical"))
setClassUnion("McmcIndex2", c("integer", "missing", "function", "logical"))
setClassUnion("McmcIndex3", c("missing", "function"))

stopifnotbool <- function(x) {
    stopifnot(is(x, "logical"))
}

extract1_mcmc_long <- function(x, i=TRUE, j=TRUE, k=TRUE, value=TRUE) {
    # It is easier to do internal dispatch than to write methods for the
    # 4^3 combinations Parameter
    if (is.character(i)) {
        i <- x[["parameter"]] == i
    } else if (is(i, "function")) { 
        i <- i(x[["parameter"]])
    }
    stopifnotbool(i)
    # Chain
    if (is(j, "character")) {
        j <- x[["chain"]] == j
    } else if (is(j, "function")) {
        j <- j(x[["chain"]])
    }
    stopifnotbool(j)
    # Iteration
    if (is(k, "character")) {
        k <- x[["iteration"]] == k
    } else if (is(k, "function")) { 
        k <- k(x[["iteration"]])
    }
    stopifnotbool(k)
    ## Value constraints
    if (is(value, "function")) {
        value <- value(x[["value"]])
    }
    stopifnotbool(value)
    # Extract
    x[i & j & k & value, ]
}

setMethod("[", signature(x="McmcLong",
                         i="McmcIndex1",
                         j="McmcIndex2"),
          extract1_mcmc_long)

setMethod("[[", signature(x="McmcLong", i="character"),
          function(x, i) x[x[["parameter"]] == i, ])


setMethod("$", signature(x="McmcLong"),
          function(x, name) x[[name]])

          

