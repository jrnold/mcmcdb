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
#' @param pararrays \code{character}. Parameter arrays to include. If \code{NULL}, all parameter arrays.
#' The union of flat parameters in \code{pararrays} and \code{flatpars} is included.
#' @param chain_id \code{integer}. Chains to include. If \code{NULL}, all chains.
#' @param iter \code{integer}. Iterations to include. If \code{NULL}, all iterations.
#' @param ... Options passed to internal functions.
#'
#' @return \code{list}. Each element of the list is a \code{list} 
#' of \code{numeric} vectors, representing the samples of each flat parameter in each chain.
#'
#' @examples
#' data(line_samples)
#' line_samples_fc <- mcmcdb_samples_flatpars_chain(line_samples)
#' str(line_samples_fc)
setGeneric("mcmcdb_samples_flatpars_chain",
           function(object, ...) {
             standardGeneric("mcmcdb_samples_flatpars_chain")
           })

#' @rdname mcmcdb_samples_flatpars_chain-methods
#' @aliases mcmcdb_samples_flatpars_chain,Mcmcdb-method
#' @family Mcmcdb methods
setMethod("mcmcdb_samples_flatpars_chain", "Mcmcdb",
          function(object, flatpars=NULL, pararrays=NULL,
                   iter=NULL, chain_id=NULL, ...) {
            if (is.null(chain_id)) {
              chain_id <- mcmcdb_chains(object)
            }
            flatpars <-
              mcmcdb_wide_select_params2(object,
                                         flatpars = flatpars,
                                         pararrays = pararrays)
            names(flatpars) <- flatpars
            .fun <- function(par) {
              .fun2 <- function(i) {
                mcmcdb_samples_flatpars(object, flatpars = par,
                                        chain_id = i, drop=TRUE)
              }
              llply(chain_id, .fun = .fun2)
            }
            llply(flatpars, .fun = .fun, ...)
          })
