#' @include package.R
#' @include class-McmcdbParameters.R
#' @exportMethod mcmcdb_pararrays
NULL

#' Get parameter array names
#'
#' @param object Object
#' @rdname mcmcdb_pararrays-methods
#' @name mcmcdb_pararrays-methods
#' @return Named \code{list} of \code{character} vectors. Names are the parameter
#' array names; element values are the associated flat parameter names.
#'
#' @aliases mcmcdb_pararrays
#' @aliases mcmcdb_pararrays-methods
NULL
setGeneric("mcmcdb_pararrays",
           function(object, ...) standardGeneric("mcmcdb_pararrays"))

mcmcdb_pararrays.McmcdbParameters <- function(object) {
  llply(object@pararrays, slot, name = "flatpars")
}

#' @rdname mcmcdb_pararrays-methods
#' @aliases mcmcdb_pararrays,McmcdbParameters-method
#' @family McmcdbParameters methods
setMethod("mcmcdb_pararrays", "McmcdbParameters",
          mcmcdb_pararrays.McmcdbParameters)

#' @rdname mcmcdb_pararrays-methods
#' @aliases mcmcdb_pararrays,McmcdbWide-method
#' @family McmcdbWide methods
setMethod("mcmcdb_pararrays", "McmcdbWide",
          function(object) callGeneric(object@parameters))
