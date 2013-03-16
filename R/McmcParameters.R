#' @include utilities.R
NULL

#' McmcFlatpar class
#'
#' Class representing attributes of a flattened MCMC parameter: the
#' name of its parameter array and its index in that array. 
#'
#' @section Slots:
#' \describe{
#' \item{\code{pararray}}{\code{"character"}. Name of parameter array}
#' \item{\code{index}}{\code{"index"}. Index of parameter}
#' }
#' @docType class
#' @name McmcFlatpar-class
#' @aliases McmcFlatpar
#' @aliases McmcFlatpar-class
#' @seealso \linkS4class{McmcFlatparList} for a list of \code{McmcFlatpar} objects.
#' @exportClass McmcFlatpar
#' @export McmcFlatpar
#' @examples
#' # beta[1,1]
#' showClass("McmcFlatpar")
#' McmcFlatpar(pararray="beta", index=c(1L, 1L))
NULL
McmcFlatpar <-
  setClass("McmcFlatpar",
           representation(pararray = "character",
                          index = "integer"))

setMethod("show", "McmcFlatpar",
          function(object) {
            cat(sprintf("An object of class %s: %s[%s]\n", dQuote("McmcFlatpar"),
                    object@pararray,
                        paste(object@index, collapse=",")))
          })

setMethod("initialize", "McmcFlatpar",
          function(.Object, pararray, index) {
            .Object@pararray <- as.character(pararray)
            .Object@index <- as.integer(index)
            validObject(.Object)
            .Object
          })

#' McmcFlatparList class
#'
#' A list of \linkS4class{McmcFlatpar} objects.
#'
#' @section Extends:
#' \describe{
#' \item{\code{namedList}}{directly}
#' }
#' @seealso \linkS4class{McmcFlatpar}
#' @name McmcFlatparList-class
#' @aliases McmcFlatparList-class
#' @aliases McmcFlatparList
#' @docType class
#' @keywords internal
#' @exportClass McmcFlatparList
#' @export McmcFlatparList
#' @examples
#' showClass("McmcFlatparList")
#' flatpars <-
#'   structure(mapply(function(x, y) {
#'    McmcFlatpar(pararray=x, index=y)
#'   }, "beta", 1:2, SIMPLIFY=FALSE),
#'            .Names = paste0("beta[", 1:2, "]"))
#' McmcFlatparList(flatpars)                         
NULL
McmcFlatparList <-
  subclass_homog_list("McmcFlatparList", "McmcFlatpar")

#' McmcPararray Class
#'
#' An S4 class containing metadata for an MCMC parameter array:
#' its dimensions, and the names of the flattened parameters within it.
#'
#' @section Slots:
#' \describe{
#' \item{\code{flatpars}}{\code{"character"}. Name of flattened parameters in the parameter array}
#' \item{\code{dim}}{\code{"integer"}. Dimension of the array}
#' }
#' @docType class
#' @name McmcPararray-class
#' @aliases McmcPararray-class
#' @aliases McmcPararray
#' @keywords internal
#' @exportClass McmcPararray
#' @export McmcPararray
#' @examples
#' showClass("McmcPararray")
#' McmcPararray(flatpars=paste0("beta[", 1:2L, "]"), dim=2L)
McmcPararray <-
  setClass("McmcPararray",
           representation(dim = "integer", flatpars = "character"))

mcmc_pararray_validity <- function(object) {
  if (length(object@flatpars) != prod(object@dim)) {
    return("length(object@flatpars) != prod(object@dims)")
  }
  TRUE
}

setValidity("McmcPararray", mcmc_pararray_validity)

setMethod("show", "McmcPararray",
          function(object) {
            cat(sprintf("An object of class %s\nDim (%s): %s",
                        dQuote("McmcPararray"),
                        paste(object@dim, collapse=","),
                        ifelse(length(object@flatpars) == 1,
                               object@flatpars[1],
                               paste(object@flatpars[1], "..."))), "\n")
          })

setMethod("initialize", "McmcPararray",
          function(.Object, dim, flatpars) {
            .Object@dim <- as.integer(dim)
            .Object@flatpars <- as.character(flatpars)
            validObject(.Object)
            .Object
          })

#' McmcPararrayList Class
#'
#' A list of \linkS4class{McmcPararray} objects.
#'
#' @section Extends:
#' \describe{
#' \item{\code{namedList}}{directly.}
#' }
#' @docType class
#' @name McmcPararrayList-class
#' @aliases McmcPararrayList-class
#' @aliases McmcPararrayList
#' @keywords internal
#' @seealso \code{\link{McmcPararray}}
#' @exportClass McmcPararrayList
#' @export McmcPararrayList
#' @examples
#' showClass("McmcPararrayList")
#' mcmcpararrays <-
#'  list(beta = McmcPararray(flatpars=paste0("beta[", 1:2L, "]"), dim=2L),
#'       alpha = McmcPararray(flatpars="alpha", dim=1L))
#' McmcPararrayList(mcmcpararrays)
McmcPararrayList <-
  subclass_homog_list("McmcPararrayList", "McmcPararray")

#' McmcParameters Class
#'
#' Metadata about MCMC parameters, including the names
#' of flattened parameters, mapping between flattened parameter
#' names and parameter array names, and the dimenions of
#' parameter arrays.
#'
#' Objects of this class are usually created by \code{\link{mcmc_parse_parnames}}.
#'
#' @section Slots:
#' \describe{
#' \item{\code{flatpars}}{An object of \code{\link{McmcFlatparList}}.
#' Names and information on the flattened parameters.}
#' \item{\code{pararrays}}{An object of \code{\link{McmcPararrayList}}.
#' Names and information on the parameter arrays.}
#' }
#' 
#' @section Methods:
#' \describe{
#'  \item{dim}{\code{signature(x = "McmcParameters")}:
#'  Returns a named \code{integer} vector with the number of flat parameters and parameter arrays.}    
#'  \item{dimnames}{\code{signature(x = "McmcParameters")}:
#'  Returns a \code{list} with names of the flat parameters and parameter arrays.}
#' }
#' 
#' @docType class
#' @name McmcParameters-class
#' @aliases McmcParameters
#' @aliases McmcParameters-class
#' @aliases dim,McmcParameters-method
#' @aliases dimnames,McmcParameters-method
#' @keywords internal
#' @seealso \code{\link{mcmc_parse_parnames}}
#' @exportClass McmcParameters
#' @export McmcParameters
#' @examples
#' showClass("McmcParameters")
McmcParameters <-
  setClass("McmcParameters",
           representation(flatpars = "McmcFlatparList",
                          pararrays = "McmcPararrayList"))
                          
setMethod("dimnames", "McmcParameters",
          function(x) {
            list(flatpars = names(x@flatpars),
                 pararrays = names(x@pararrays))
          })

setMethod("dim", "McmcParameters",
          function(x) {
            c(flatpars = length(x@flatpars),
              pararrays = length(x@pararrays))
          })

show_McmcParameters <- function(object) {
  cat(sprintf("An object of class %s\n", dQuote("McmcParameters")))
  cat("Parameters:\n")
  for (i in seq_along(object@pararrays)) {
    parname <- names(object@pararrays)[i]
    pardim <- dim(object@pararrays[[i]])
    cat(sprintf("$ %s [%s]\n",
                parname, paste(pardim, collapse=",")))
  }
}

setMethod("show", "McmcParameters", show_McmcParameters)


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
#' @export
setGeneric("mcmcdb_par_indices",
           function(object, ...) standardGeneric("mcmcdb_par_indices"))

mcmcdb_par_indices.McmcParameters <- function(object) {
  llply(object@pararrays,
        function(pa, flatpars) {
          ## TODO. Expand indices
          ind <- laply(pa@flatpars, function(i) flatpars[[i]]@index, .drop = FALSE)
          rownames(ind) <- pa@flatpars
          ind
        }, flatpars = object@flatpars)
}

#' @rdname mcmcdb_par_indices-methods
#' @aliases mcmcdb_par_indices,McmcParameters-method
setMethod("mcmcdb_par_indices", "McmcParameters",
          mcmcdb_par_indices.McmcParameters)

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
#' @export
setGeneric("mcmcdb_flatpars",
           function(object, ...) standardGeneric("mcmcdb_flatpars"))

mcmcdb_flatpars.McmcParameters <- function(object) {
  unlist(llply(object@flatpars, slot, name = "pararray"))
}

#' @rdname mcmcdb_flatpars-methods
#' @aliases mcmcdb_flatpars,McmcParameters-method
setMethod("mcmcdb_flatpars", "McmcParameters",
          mcmcdb_flatpars.McmcParameters)

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
#' @export
setGeneric("mcmcdb_pararrays",
           function(object, ...) standardGeneric("mcmcdb_pararrays"))

mcmcdb_pararrays.McmcParameters <- function(object) {
  llply(object@pararrays, slot, name = "flatpars")
}

#' @rdname mcmcdb_pararrays-methods
#' @aliases mcmcdb_pararrays,McmcParameters-method
setMethod("mcmcdb_pararrays", "McmcParameters",
          mcmcdb_pararrays.McmcParameters)


#' Get parameter array dimensions
#'
#' @param object Object
#' @rdname mcmcdb_pardims-methods
#' @name mcmcdb_pardims-methods
#' @return Named \code{list} of \code{integer} vectors. Names are the parameter
#' array names; element values are the associated dimensions of the arrays.
#'
#' @aliases mcmcdb_pardims
#' @aliases mcmcdb_pardims-methods
#' @export
setGeneric("mcmcdb_pardims",
           function(object, ...) standardGeneric("mcmcdb_pardims"))

mcmcdb_pardims.McmcParameters <- function(object) {
  llply(object@pararrays, slot, name = "dim")
}

#' @rdname mcmcdb_pardims-methods
#' @aliases mcmcdb_pardims,McmcParameters-method
setMethod("mcmcdb_pardims", "McmcParameters",
          mcmcdb_pardims.McmcParameters)
