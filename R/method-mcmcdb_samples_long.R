#' @include package.R
#' @include class-McmcdbWide.R
#' @include mcmcdb_wide_misc.R
#' @exportMethod mcmcdb_samples_long
NULL

#' @rdname mcmcdb_samples_long-methods
#' @docType methods
#' @title Extract MCMC Samples (Long form)
#'
#' @description Extract MCMC samples from an object as a data frame, with
#' columns for the flat parameter, chain, iteration, and
#' value.
#'
#' @param object An object containing the MCMC samples.
#' @param flatpars \code{character}. Flat parameters to include. If \code{NULL}, all flat parameters.
#' @param parameters \code{character}. Parameter arrays to include. If \code{NULL}, all parameter arrays.
#' The union of flat parameters in \code{parameters} and \code{flatpars} is included.
#' @param chain_id \code{integer}. Chains to include. If \code{NULL}, all chains.
#' @param iter \code{integer}. Iterations to include. If \code{NULL}, all iterations.
#' @param ... Options passed to internal functions.
#'
#' @return A \code{data.frame} with columns
#' \describe{
#' \item{\code{chain_id}}{\code{integer}. Chain id}
#' \item{\code{iter}}{\code{integer}. Iteration number}
#' \item{\code{flatpar}}{\code{factor}. Flat parameter name}
#' \item{\code{val}}{\code{numeric}. Parameter values}
#' }
#' @examples
#' library(plyr)
#' data(line_samples)
#' line_long <- mcmcdb_samples_long(line_samples)
#' head(line_long)
#' summary(line_long)
#' ddply(line_long, "flatpar",
#'       summarise, mean = mean(val), median = median(val))
setGeneric("mcmcdb_samples_long",
           function(object, ...) {
             standardGeneric("mcmcdb_samples_long")
           })

#' @rdname mcmcdb_samples_long-methods
#' @aliases mcmcdb_samples_long,McmcdbWide-method
#' @family McmcdbWide methods
setMethod("mcmcdb_samples_long", "McmcdbWide",
          function(object, flatpars=NULL, parameters=NULL, iter=NULL,
                   chain_id=NULL) {
            params <- mcmcdb_wide_select_params(object,
                                                flatpars = flatpars,
                                                parameters = parameters)
            iters <- mcmcdb_wide_select_iters(object,
                                              iter = iter,
                                              chain_id = chain_id)
            x <- mcmcdb_wide_subset(object,
                                    flatpars=flatpars, parameters=parameters,
                                    iter=iter, chain_id=chain_id, drop=FALSE)
            x <- melt(cbind(as.data.frame(x),
                            as.data.frame(object@iters[iters, ])),
                      id.vars = c("chain_id", "iter"),
                      variable.name = "flatpar", value.name = "val")
          })
