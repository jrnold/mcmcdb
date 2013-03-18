#' @include package.R
#' @include class-McmcdbParameters.R
#' @exportMethod mcmcdb_par_indices
NULL

#' Get parameter indices
#'
#' @param object Object
#' @return \code{list} of integer matrices. Each element of the
#' list is a parameter array. Each matrix has a number of rows equal to the
#' total number of flat parameters, and a number of columns equal
#' to the number of dimensions of the parameter array. The rownames of
#' each matrix are the flat parameter names.
#'
#' @rdname mcmcdb_par_indices-methods
#' @name mcmcdb_par_indices-methods
#' @aliases mcmcdb_par_indices
#' @aliases mcmcdb_par_indices-methods
NULL
setGeneric("mcmcdb_par_indices",
           function(object, ...) standardGeneric("mcmcdb_par_indices"))

mcmcdb_par_indices.McmcdbParameters <- function(object) {
  llply(object@pararrays,
        function(pa, flatpars) {
          ## TODO. Expand indices
          ind <- laply(pa@flatpars, function(i) flatpars[[i]]@index, .drop = FALSE)
          rownames(ind) <- pa@flatpars
          ind
        }, flatpars = object@flatpars)
}

#' @rdname mcmcdb_par_indices-methods
#' @aliases mcmcdb_par_indices,McmcdbParameters-method
#' @family McmcdbParameters methods
setMethod("mcmcdb_par_indices", "McmcdbParameters",
          mcmcdb_par_indices.McmcdbParameters)

#' @rdname mcmcdb_par_indices-methods
#' @aliases mcmcdb_par_indices,McmcdbParameters-method
#' @family McmcdbWide methods
setMethod("mcmcdb_par_indices", "McmcdbWide",
          function(object) callGeneric(object@parameters))
