#' @include package.R
#' @include class-Mcmcdb.R
#' @include method-mcmcdb_chains.R
#' @include method-mcmcdb_iters.R
#' @exportMethod mcmcdb_mcpar
NULL

#' @rdname mcmcdb_mcpar-methods
#' @name mcmdb_mcpar-methods
#' @aliases mcmcdb_mcpar
#' @docType methods
#' @title Methods for function \code{mcmcdb_par}
#'
#' @description Get basic information about each chain: number of iterations,
#' first iteration, last iteration, thinning.
#' 
#' @param object Object
#' @return \code{data.frame} with columns
#' \describe{
#' \item{\code{chain_id}}{Chain id}
#' \item{\code{n_iter}}{\code{integer}. Number of iterations}
#' \item{\code{iter_start}}{\code{integer}. First iteration saved. Can be missing}
#' \item{\code{iter_end}}{\code{integer}. Last iteration saved. Can be missing}
#' \item{\code{iter_thin}}{\code{integer}. Thinning saved. Can be missing}
#' }
#' @section Methods:
#' \describe{
#' \item{\code{signature(object = "Mcmcdb")}}{}
#' }
#' @examples
#' data(line_samples)
#' mcmcdb_mcpar(line_samples)
setGeneric("mcmcdb_mcpar",
           function(object, ...) {
             standardGeneric("mcmcdb_mcpar")
           })

#' @rdname mcmcdb_mcpar-methods
#' @aliases mcmcdb_mcpar,Mcmcdb-method
#' @family Mcmcdb methods
setMethod("mcmcdb_mcpar", "Mcmcdb", 
          function(object) {
            chains <- mcmcdb_chains(object, drop=FALSE)
            mcpar <- as(chains, "data.frame")[ , "chain_id", drop=FALSE]
            if ("n_iter" %in% names(chains)) {
              mcpar[["n_iter"]] <- chains[["n_iter"]]
            } else {
              n_iters <-
                ddply(mcmcdb_iters(object), "chain_id",
                      function(x) data.frame(n_iter = nrow(x)))
              mcpar <- merge(mcpar, n_iters)
            }
            for (i in c("iter_start", "iter_end", "iter_thin")) {
              if (i %in% chains) {
                mcpar[[i]] <- chains[[i]]
              } else {
                mcpar[[i]] <- NA_integer_
              }
            }
            mcpar
          })
