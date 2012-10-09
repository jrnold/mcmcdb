## Sum of squared errors
sumsqe <- function(x)  sum((x - mean(x))^2)

## Matrix version
gelman_diag_matrix <- function(x) {
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

## coda::gelman.diag not generalizing for ragged chains
## this is the vector version
gelman_diag <- function(x)  {
    m <- length(x)
    n <- nrow(x[[1]])
    mean_w <- sapply(x, mean)
    B <- (n / (m - 1)) * apply(mean_w, 1, sumsqe)
    W <- apply(sapply(x, function(y) apply(y, 2, var)), 1, mean)
    vartheta <- ((n - 1) / n) * W + (1 / n) * B
    pmax(1, sqrt(vartheta / W))
}

##' Gelman-Rubin Criteria
##'
##' @param x object representing a single chain.
##'
##' @references Stan Manual, Section 27.2.
##'
##' @aliases gelmanDiag
##' @aliases gelmanDiag-method
##' @keywords methods
##' @docType methods
##' @export
setGeneric("gelmanDiag", gelman_diag_matrix)

