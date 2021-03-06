#' @include package.R
#' @include class-McmcdbParameters.R
#' @include class-Mcmcdb.R
#' @include method-mcmcdb_parameters.R
#' @exportMethod mcmcdb_flatpars
NULL

#' Get flat parameter names
#'
#' @param object Object
#' @return Named \code{character} vector. Names are the flat parameter names;
#' values are the associated parameter arrays.
#'
#' @name mcmcdb_flatpars-methods
#' @rdname mcmcdb_flatpars-methods
#' @aliases mcmcdb_flatpars
#' @aliases mcmcdb_flatpars-method
setGeneric("mcmcdb_flatpars",
           function(object, ...) standardGeneric("mcmcdb_flatpars"))

mcmcdb_flatpars.McmcdbParameters <- function(object) {
  flatpars <- unlist(object)
  parameters <-
    unlist(llply(seq_along(object),
                 function(i) {
                   rep(names(object)[i], length(object[[i]]))
                 }))
  names(parameters) <- flatpars
  parameters
}

#' @rdname mcmcdb_flatpars-methods
#' @aliases mcmcdb_flatpars,McmcdbParameters-method
#' @family McmcdbParameters methods
setMethod("mcmcdb_flatpars", "McmcdbParameters",
          mcmcdb_flatpars.McmcdbParameters)

#' @rdname mcmcdb_flatpars-methods
#' @aliases mcmcdb_flatpars,Mcmcdb-method
#' @family Mcmcdb methods
setMethod("mcmcdb_flatpars", "Mcmcdb",
          function(object) callGeneric(mcmcdb_parameters(object)))
