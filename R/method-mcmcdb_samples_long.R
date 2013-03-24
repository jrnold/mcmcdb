#' @include package.R
#' @include class-McmcdbWide.R
#' @include mcmcdb_wide_misc.R
#' @exportMethod mcmcdb_samples_long
NULL

#' @rdname mcmcdb_samples_long-methods
#' @title Mcmcdb Object samples (Long form)
#'
#' @description Return samples from a Mcmcdb
#' object in a matrix (iterations x long).
#' 
#' @return \code{data.frame} with columns
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
          function(object, flatpars=NULL, pararrays=NULL, iter=NULL,
                   chain_id=NULL) {
            params <- mcmcdb_wide_select_params(object,
                                                flatpars = flatpars,
                                                pararrays = pararrays)
            iters <- mcmcdb_wide_select_iters(object,
                                              iter = iter,
                                              chain_id = chain_id)
            x <- mcmcdb_wide_subset(object,
                                    flatpars=flatpars, pararrays=pararrays,
                                    iter=iter, chain_id=chain_id, drop=FALSE)
            melt(cbind(as.data.frame(x),
                       as.data.frame(object@iters[iters, ])),
                 id.vars = c("chain_id", "iter"),
                 variable.name = "flatpar", value.name = "val")
          })
