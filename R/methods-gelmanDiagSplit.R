##' Gelman-Rubin Diagnostic with a Split Single Chain
##'
##' The single chain is split in half, and then
##' \code{gelman_diag} is run on the two halves.
##' 
##' @param x Object containing the MCMC samples.
##'
##'
##' @references Stan Manual, Section 27.2.
##'
##' @aliases gelmanDiagSplit
##' @aliases gelmanDiagSplit-method
##' @aliases gelmanDiagSplit,numeric-method
##' @aliases gelmanDiagSplit,matrix-method
##' @aliases gelmanDiagSplit,list-method
##' @aliases gelmanDiagSplit,mcmc-method
##' @docType methods
##' @keywords methods
##' @export
setGeneric("gelmanDiagSplit",
           function(x, ...) {
               standardGeneric("gelmanDiagSplit")
           })

setMethod("gelmanDiagSplit", "numeric",
          function(x, ...) {
              n <- length(x)
              i1 <- 1:floor(n/2)
              i2 <- ceiling(n/2 + 1):n
              gelman_diag(mcmc.list(mcmc(x[i1]), mcmc(x[i2])))
          })

setMethod("gelmanDiagSplit", "matrix",
          function(x, ...) {
              apply(x, 2, gelmanDiagSplit)
          })

setMethod("gelmanDiagSplit", "list",
          function(x, ...) {
              lapply(x, gelmanDiagSplit, ...)
          })

setMethod("gelmanDiagSplit", "mcmc",
          function(x, ...) {
              setGeneric(as.matrix(x), ...)
          })

