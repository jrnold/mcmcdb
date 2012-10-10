##' Methods for function \code{melt}
##'
##' Melt MCMC samples objects into a \code{data.frame} with columns for the
##' chain, iteration, parameter, and value.
##'
##'
##' @param data Object containing the MCMC samples.
##' @param chain Set chain number; for \code{mcmc} objects.
##'
##' @section Methods:
##'
##' \describe{
##' \item{\code{signature(data="mcmc")}}{Method for \code{mcmc}}
##' \item{\code{signature(data="mcmc.list")}}{Method for \code{mcmc.list}}
##' }
##'
##' @return \code{data.frame} with columns
##' \describe{
##' \item{\code{chain}}{\code{integer}}
##' \item{\code{iteration}}{\code{integer}}
##' \item{\code{parameter}}{\code{factor}}
##' \item{\code{value}}{\code{numeric}}
##' }
##'
##' @name melt-methods
##' @rdname melt-methods
##' @aliases melt,mcmc-method melt,mcmc.list-method
##' @seealso \code{\link[reshape2]{melt}} for the generic function.
NULL

##' @export
setGeneric("melt", reshape2::melt)

##' @export
setMethod("melt", "mcmc",
    function(data, chain=1) {
        result <- callGeneric(as(data, "matrix"))
        colnames(result)[1:2] <- c("iteration", "parameter")
        result$chain <- chain
        rownames(result) <- with(result, paste(parameter, chain, iteration, sep="."))
        result[ , c("parameter", "chain", "iteration", "value")]
    })

##' @export
setMethod("melt", "mcmc.list",
          function(data) {
              chains <- length(data)
              data_len <- laply(data, function(x) prod(dim(x)))
              result <- mapply(function(x, i) melt(x, chain=i),
                               data, seq_along(data), SIMPLIFY=FALSE)
              result <- do.call(rbind, result)
              rownames(result) <- with(result, paste(parameter, chain, 
                                                     iteration, sep="."))
              result[ , c("parameter", "chain", "iteration", "value")]
          })

