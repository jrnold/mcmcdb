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
NULL
McmcFlatpar <-
  setClass("McmcFlatpar",
           representation(pararray = "character",
                          index = "integer"))

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
McmcPararray <-
  setClass("McmcPararray",
           representation(flatpars = "character",
                          dim = "integer"))

mcmc_pararray_validity <- function(object) {
  if (length(object@flatpars) != prod(object@dims)) {
    return("length(object@flatpars) != prod(object@dims)")
  }
  TRUE
}

setValidity("McmcPararray", mcmc_pararray_validity)

#' McmcPararray Class
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
McmcPararrayList <-
  subclass_homog_list("McmcPararrayList", "McmcPararray")

#' McmcParameters Class
#'
#' Metadata about MCMC parameters, including the names
#' of flattened parameters, mapping between flattened parameter
#' names and parameter array names, and the dimenions of
#' parameter arrays.
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
#' @aliases MamcParameters
#' @aliases MamcParameters-class
#' @aliases dim,McmcParameters-method
#' @aliases dimnames,McmcParameters-method
#' @keywords internal
#' @exportClass McmcParameters
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

#' Parse MCMC parameter names
#'
#' @param x \code{character} vector with flat parameter names.
#' @return Object of class \code{McmcFlatparList}
#'
#' @rdname mcmc_parparsers
#' @export
mcmc_parparser_scalar <- function(x) {
  ret <- mapply(function(pararray, index)
                McmcFlatpar(pararray=pararray, index=index),
         x, 1, SIMPLIFY=FALSE)
  names(ret) <- x
  McmcFlatparList(ret)
}

##' @rdname mcmc_parparsers
##' @export
mcmc_parparser_stan <- function(x) {
  x2 <- str_split_fixed(x, fixed("."), 2)
  indices <- llply(str_split(x2[ , 2], fixed(".")),
                   function(x) ifelse(x == "", 1, as.integer(x)))
  ret <- mapply(function(pararray, index)
                McmcFlatpar(pararray=pararray, index=index),
                x2[ , 1], indices, SIMPLIFY=FALSE)
  names(ret) <- x
  McmcFlatparList(ret)
}

##' @rdname mcmc_parparsers
##' @export
mcmc_parparser_bugs <- function(x) {
  x2 <- str_match(x, "([^\\[]+)(\\[([0-9,]+)\\])?")[ , c(2, 4)]
  indices <- llply(str_split(x2[ , 2], fixed(",")),
                   function(x) ifelse(x == "", 1, as.integer(x)))
  ret <- mapply(function(pararray, index)
                McmcFlatpar(pararray=pararray, index=index),
                x2[ , 1], indices, SIMPLIFY=FALSE)
  names(ret) <- x
  McmcFlatparList(ret)
}

# @param x \code{McmcFlatparList}
create_pararrays <- function(x) {
  xpars <- sapply(x, slot, "pararray")
  pararrays <- unique(xpars)
  pardims <-
    lapply(pararrays,
           function(i) {
             apply(do.call(rbind, lapply(x[xpars == i], slot, "index")),
                   2, max)
           })
  flatpars <- lapply(pararrays, function(i) names(xpars)[xpars == i])
  ret <- McmcPararrayList(mapply(function(x, y) {
    McmcPararray(dim = x, flatpars = y)
  }, pardims,  flatpars))
  names(ret) <- pararrays
  ret
}

#' Create McmcParameter object from MCMC parameter names
#'
#' @param x \code{character} vector of parameter names
#' @param parser \code{function} parse \code{x} into \code{McmcFlatparList}.
#' @return Object of class \code{McmcParameters}
mcmc_parse_parnames <- function(x, parser = mcmc_parparser_bugs) {
  flatpars <- parser(x)
  pararrays <- create_pararrays(flatpars)
  McmcParameters(flatpars = flatpars, pararrays = pararrays)
}

#' Check MCMC parameter names
#'
#' Check flat parameter names to see if they are consistent
#' with the parameter names generated by BUGS or Stan.
#'
#' @param x \code{character} vector of flat parameter names
#' @param style Eiter \code{"bugs"} or \code{"stan"}, to check
#' if the parameters are valid BUGS/JAGS or Stan names, respectively.
#' @return \code{logical}. 
#' @rdname valid_mcmc_parnames
#' @export
valid_mcmc_parnames <- function(x, style="bugs") {
  if (! style %in% c("bugs", "stan")) {
  }
  if (style == "bugs") {
    str_match_all(x, "^[A-Za-z.][A-Za-z.0-9]*(\\[\\d(,\\d)*\\])?$")
  } else if (style == "stan") {
    str_match_all(x, "^[A-Za-z][A-Za-z0-9_]*(\\.\\d)*$")
  } else {
    stop("Option %s must be either %s or %s",
         sQuote("style"), dQuote("bugs"), dQuote("stan"))
  }
}

#' Create Unlisted Parameter Names
#'
#' Given a parameter name and a matrix of index values, generate
#' names for the unlisted parameters.
#'
#' @param x \code{character}. Parameter name.
#' @param dim \code{integer} dimension of the array.
#'
#' @rdname mcmc_create_parnames
#' @aliases mcmc_create_parnames_stan
#' 
#' @export
mcmc_create_parnames_stan <- function(x, dim) {
  if (identical(as.integer(dim), 1L)) {
    x
  } else {
    idxstr <- apply(expand_grid_dim(dim), 1, paste, collapse=".")
    paste(x, idxstr, sep=".")
  }
}

mcmc_create_parnames_stan_idx <- function(x, idx, dim) {
  if (identical(as.integer(dim), 1L)) {
    x
  } else {
    paste(x, paste(idx, collapse="."), sep=".")
  }
}

#' Create MCMC parnames
#'
#' @rdname mcmc_create_parnames
#' @aliases mcmc_create_parnames_bugs
#' @export
mcmc_create_parnames_bugs <- function(x, dim) {
  if (identical(as.integer(dim), 1L)) {
    x
  } else {
    idxstr <- apply(expand_grid_dim(dim), 1, paste, collapse=",")
    paste0(x, "[", idxstr, "]")
  }
}

mcmc_create_parnames_bugs_idx <- function(x, idx) {
  if (identical(as.integer(dim), 1L)) {
    x
  } else {
    paste0(x, "[", paste(idx, collapse=","), "]")
  }
}


## bugs_to_stan_parnames <- function(x) {
##   gsub("]", "", gsub("[\\[,]", ".", x))
## }

## stan_to_bugs_parnames <- function(x) {
##   y <- str_split_fixed(x, fixed("."), 2)
##   paste0(y[ ,1], "[", gsub("\\.", ",", y[ , 2]), "]")
## }

