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
    McmcSamples(xsub[ , c("flatpar", "chain_id", "iter", "val")])
  }
}

#' @export
setMethod("[", c(x = "McmcdbWide"), `[.McmcdbWide`)

`[[.McmcdbWide` <- function(x, i, j, k, drop=TRUE) {
  ## Restrict to 1 parameter for now
  i <- i[1]
  
  
}
