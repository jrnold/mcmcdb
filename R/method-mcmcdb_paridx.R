#' @include package.R
#' @include utilities.R
#' @include class-McmcdbParameters.R
#' @include class-Mcmcdb.R
#' @include method-mcmcdb_parameters.R
#' @exportMethod mcmcdb_paridx
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
#' @rdname mcmcdb_paridx-methods
#' @name mcmcdb_paridx-methods
#' @aliases mcmcdb_paridx
#' @aliases mcmcdb_paridx-methods
setGeneric("mcmcdb_paridx",
           function(object, ...) standardGeneric("mcmcdb_paridx"))

mcmcdb_paridx.McmcdbParameters <- function(object) {
  # llply returns matrix (why?)
  ret <- lapply(object,
                function(x) {
                  d <- dimlen(x)
                  idx <- expand_grid_dim(d)
                  rownames(idx) <- as.character(x)
                  idx
                })
  names(ret) <- names(object)
  ret
}

#' @rdname mcmcdb_paridx-methods
#' @aliases mcmcdb_paridx,McmcdbParameters-method
#' @family McmcdbParameters methods
setMethod("mcmcdb_paridx", "McmcdbParameters",
          mcmcdb_paridx.McmcdbParameters)

#' @rdname mcmcdb_paridx-methods
#' @aliases mcmcdb_paridx,Mcmcdb-method
#' @family Mcmcdb methods
setMethod("mcmcdb_paridx", "Mcmcdb",
          function(object) callGeneric(mcmcdb_parameters(object)))
