## Sum of squared errors
sumsqe <- function(x)  sum((x - mean(x))^2)

gelman_diag <- function(x, ...) {
    x <- lapply(x, as.matrix)
    m <- length(x)
    iters <- sapply(x, nrow)
    ## within chain mean
    ## dim = param, chain
    mean_w <- sapply(x, mean)
    if (!is.matrix(mean_w)) {
        mean_w <- t(as.matrix(mean_w))
    }
    ## mean of within chain variance
    ss_w <- sapply(x, function(x) apply(x, 2, sumsqe))
    if (!is.matrix(ss_w)) {
        ss_w <- t(as.matrix(ss_w))
    }
    W <- apply(ss_w, 1, function(x) mean(x / (iters - 1)))
    var1 <- apply(ss_w, 1, function(x) mean(x / iters))
    var2 <- apply(mean_w, 1, var)
    vartheta <- var1 + var2
    Rhat <- pmax(1, sqrt(vartheta / W))
    Rhat
}

## Gelman diag not generalizing for ragged chains
## gelman_diag <- function(x)  {
##     m <- length(x)
##     n <- nrow(x[[1]])
##     mean_w <- sapply(x, mean)
##     B <- (n / (m - 1)) * apply(mean_w, 1, sumsqe)
##     W <- apply(sapply(x, function(y) apply(y, 2, var)), 1, mean)
##     vartheta <- ((n - 1) / n) * W + (1 / n) * B
##     pmax(1, sqrt(vartheta / W))
## }

##' Gelman-Rubin Criteria
##'
##' @param x object representing a single chain.
##'
##' @references Stan Manual, Section 27.2.
##'
##' @export
setGeneric("gelman_diag", gelman_diag)


##' Gelman-Rubin Diagnostic with a Split Single Chain
##'
##' @param x Object containing the MCMC samples.
##'
##' The single chain is split in half, and then
##' \code{gelman_diag} is run on the two halves.
##'
##' @references Stan Manual, Section 27.2.
##'
##' @export
setGeneric("gelman_diag_split", function(x, ...) standardGeneric("gelman_diag_split"))

##' @export
setMethod("gelman_diag_split", "numeric",
          function(x, ...) {
              n <- length(x)
              i1 <- 1:floor(n/2)
              i2 <- ceiling(n/2 + 1):n
              gelman_diag(mcmc.list(mcmc(x[i1]), mcmc(x[i2])))
          })

##' @export
setMethod("gelman_diag_split", "matrix",
          function(x, ...) {
              apply(x, 2, gelman_diag_split)
          })

##' @export
setMethod("gelman_diag_split", "list",
          function(x, ...) {
              lapply(x, gelman_diag_split, ...)
          })

##' @export
setMethod("gelman_diag_split", "mcmc",
          function(x, ...) {
              gelman_diag_split(as.matrix(x), ...)
          })

