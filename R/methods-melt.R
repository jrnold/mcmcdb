##' Methods for function \code{melt}
##'
##' Melt MCMC samples objects into a \code{data.frame} with columns for the
##' chain, iteration, parameter, and value.
##'
##' \S4method{melt}{mcmc}{data, ...}
##'
##' \S4method{melt}{mcmc.list}{data, ...}
##'
##' @param data Object containing the MCMC samples.
##' @param chain Set chain number; for \code{mcmc} objects.
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
##' @family mcmc mcmc.list
##' @seealso \code{\link[reshape2]{melt}} for the generic function.
NULL

##' @export
setMethod("melt", "mcmc",
    function(data, chain=1) {
        result <- callGeneric(as(data, "matrix"))
        colnames(result)[1:2] <- c("iteration", "parameter")
        rownames(result) <- with(result, paste(parameter, iteration, sep="."))
        result$chain <- chain
        result
    })

##' @export
setMethod("melt", "mcmc.list",
          function(data) {
              chains <- length(data)
              data_len <- laply(data, function(x) prod(dim(x)))
              result <- do.call(rbind, llply(data, melt))
              result$chain <-
                  as.integer((mapply(function(x, y) rep(x, y),
                                     seq_len(chains), data_len)))
              rownames(result) <- with(result, paste(parameter,
                                                     iteration, chain, sep="."))
              result
          })

