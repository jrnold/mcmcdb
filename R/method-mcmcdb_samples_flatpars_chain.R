#' @include package.R
#' @include class-Mcmcdb.R
#' @include mcmcdb_wide_misc.R
#' @include method-mcmcdb_chains.R
#' @include method-mcmcdb_samples_flatpars.R
#' @exportMethod mcmcdb_samples_flatpars_chain
NULL

#' @rdname mcmcdb_samples_flatpars_chain-methods
#' @docType methods
#' @title Extract MCMC samples (Flat parameters, chains)
#'
#' @description Return MCMC samples as a list of flat parameters.
#' Each flat parameter is a list of numeric vectors, by chain.
#'
#' @param object An object containing the MCMC samples.
#' @param flatpars \code{character}. Flat parameters to include. If \code{NULL}, all flat parameters.
#' @param parameters \code{character}. Parameter arrays to include. If \code{NULL}, all parameter arrays.
#' The union of flat parameters in \code{parameters} and \code{flatpars} is included.
#' @param chain_id \code{integer}. Chains to include. If \code{NULL}, all chains.
#' @param iter \code{integer}. Iterations to include. If \code{NULL}, all iterations.
#' @param FUN \code{function}. Function to apply to each flat paramter. Function of the form
#' \code{function(x)}, where \code{x} is a \code{list} of \code{numeric} vectors, representing the
#' flat parameters in each chain.
#' @param ... Options passed to internal functions.
#'
#' @return \code{list}. If \code{FUN = NULL}, then each element of the list is a \code{list} 
#' of \code{numeric} vectors, representing the samples of each flat parameter in each chain.
#' If \code{FUN != NULL}, then the elements are the results of \code{FUN}.
#'
#' @examples
#' data(line_samples)
#' line_samples_fc <- mcmcdb_samples_flatpars_chain(line_samples)
#' str(line_samples_fc)
setGeneric("mcmcdb_samples_flatpars_chain",
           function(object, ...) {
             standardGeneric("mcmcdb_samples_flatpars_chain")
           })

mcmcdb_samples_flatpars_chain.Mcmcdb <- 
  function(object, flatpars=NULL, parameters=NULL,
           iter=NULL, chain_id=NULL, FUN=identity, return_type = "l", ...) {
    if (is.null(chain_id)) {
      chain_id <- mcmcdb_chains(object)
    }
    names(chain_id) <- chain_id
    flatpars <-
      mcmcdb_wide_select_params2(object,
                                 flatpars = flatpars,
                                 parameters = parameters)
    names(flatpars) <- flatpars
    .fun <- function(par) {
      .fun2 <- function(i) {
        as.numeric(mcmcdb_samples_flatpars(object, flatpars = par,
                                           chain_id = i))
      }
      FUN(llply(chain_id, .fun = .fun2))
    }
    plyr_fun("l", return_type)(flatpars, .fun=.fun, ...)
  }

#' @rdname mcmcdb_samples_flatpars_chain-methods
#' @aliases mcmcdb_samples_flatpars_chain,Mcmcdb-method
#' @family Mcmcdb methods
setMethod("mcmcdb_samples_flatpars_chain", "Mcmcdb",
          mcmcdb_samples_flatpars_chain.Mcmcdb)
          
