#' @include package.R
#' @include method-McmcdbParameters.R
#' @export gen_mcmcdb_parameters
NULL

#' @title Generate \code{McmcdbParameters} object given parameter dims
#'
#' @description Given a list of parameter array names and dimensions, generate
#' a \code{\linkS4class{McmcdbParameters}}
#' 
#' @param x Named \code{list} of parameter array dimensions.
#' @param style \code{function} taking two arguments, parameter array name,
#' and parameter array dimension, and returning a character vector of flat
#' parameter arrays. E.g. \code{\link{mcmc_parnames_stan}}
#' and \code{\link{mcmc_parnames_bugs}}.
#' 
#' @examples
#' mcmcpars <- list(alpha = 1L, beta = 2L, gamma = c(2L, 2L))
#' gen_mcmcdb_parameters(mcmcpars)
#' gen_mcmcdb_parameters(mcmcpars, style=mcmc_parnames_bugs)
gen_mcmcdb_parameters <- function(x, style=mcmc_parnames_stan) {
  if (length(style) > 1) {
    warning("length 'style' > 1; only first element used.")
    style <- style[1]
  }
  y <- llply(seq_along(x),
             function(i) {
               array(parnamer(names(x)[i], x[[i]]),
                     x[[i]])
             })
  names(y) <- names(x)
  McmcdbParameters(y)
}
