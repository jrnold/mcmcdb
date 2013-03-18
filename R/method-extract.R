#' @include package.R
#' @include class-McmcdbWide.R
#' @exportMethod [
NULL

#' @rdname extract-methods
#' @name extract-methods
#' @title Extract
#'
#' @description The methods \code{[}, \code{[[} and \code{$} are
#' defined for \code{\linkS4class{McmcdbWide}} objects
#' to be able to select sample values by parameter (both
#' flat parameters and parameter arrays), chain, and iteration.
#'
#' @param i \code{character} Parameter name. For \code{[}, the flattened parameter name. For \code{[[}
#' and \code{$}, the parameter array name.
#' @param j \code{integer}. Chain number.
#' @param k \code{integer}. Iteration number.
#' @param drop \code{logical}. Determines the object returned. In general, if
#' \code{drop=TRUE}, then a vector or array is returned, otherwise a \code{data.frame} is returned.
#' @return TODO
NULL

`[.McmcdbWide` <- function(x, i, j, k, drop=TRUE) {
  ## flatpar indices
  if (missing(i)) {
    ii <- TRUE
  } else {
    if (is.numeric(i)) {
      ii <- as.integer(i)
    } else {
      ii <- (colnames(x@samples) %in% as.character(i))
    }
  }
  ## iterations
  if (missing(j)) {
    jj <- TRUE
  } else {
    jj <- x@iters[["chain_id"]] == j
  }
  if (missing(k)) {
    kk <- TRUE
  } else {
    kk <- x@iters[["iter"]] == k
  }
  ## select 
  xsub <- melt(x@samples[jj & kk, ii, drop=FALSE],
               varnames = c("Var1", "flatpar"),
               value.name = "val")
  if (drop) {
    xsub[["val"]]
  } else {
    chain_iters <- as(x@iters, "data.frame")[jj & kk, , drop=FALSE]
    ## Recycles over parameters
    xsub[["iter"]] <- chain_iters[["iter"]]
    xsub[["chain_id"]] <- chain_iters[["chain_id"]]
    McmcdbSamples(xsub[ , c("flatpar", "chain_id", "iter", "val")])
  }
}

#' @rdname extract-methods
#' @aliases [,McmcdbWide-method
setMethod("[", c(x = "McmcdbWide"), `[.McmcdbWide`)

###########################################################################

`[[.McmcdbWide` <- function(x, i, j, k, drop=TRUE) {
  ## Restrict to 1 parameter for now
  i <- i[1]
}

##########################################################################

`$.McmcdbWide` <- function(x, name) {
  x@samples
}
