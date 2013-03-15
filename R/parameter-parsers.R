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
            cat(sprintf("%s: %s[%s]\n", dQuote("McmcFlatpar"),
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
            cat(sprintf("%s of dimension (%s): %s",
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
  cat(sprintf("Object of class %s\n", dQuote("McmcParameters")))
  cat("Parameters:\n")
  for (i in seq_along(object@pararrays)) {
    parname <- names(object@pararrays)[i]
    pardim <- dim(object@pararrays[[i]])
    cat(sprintf("$ %s: (%s)\n",
                parname, paste(pardim, collapse=",")))
  }
}

setMethod("show", "McmcParameters", show_McmcParameters)

#' Parse MCMC parameter names
#'
#' Functions that parse a vector of flat parameter names
#' and return an object of
#'
#' These functions are provided as an argument to \code{\link{mcmc_parse_parnames}}.
#' 
#' \describe{
#' \item{\code{mcmc_parparser_bugs}}{Parses parameter names
#' in the Stan style, e.g. \code{"beta[1,1]"}}
#' \item{\code{mcmc_parparser_stan}}{Parses parameter names
#' treating each parameter as a scalar. E.g. \code{"beta.1"}
#' and \code{"beta.2"} will be treated two parameter arrays of
#' size 1.}
#' \item{\code{mcmc_parparser_stan}}{Parses parameter names
#' in the Stan style, e.g. \code{"beta.1.1"}}
#' \item{\code{mcmc_parparser_guess}}{Tries to guess the format of the parameters}
#' }
#'
#' @param x \code{character} vector with flat parameter names.
#' @return Object of class \code{McmcFlatparList}
#'
#' @rdname mcmc_parparsers
#' @aliases mcmc_parparsers_scalar
#' @seealso \code{\link{mcmc_parse_parnames}} which takes these functions an argument.
#' @export
#' @examples
#' mcmc_parparser_bugs(c("beta[1]", "beta[2]"))
#' mcmc_parparser_stan(c("beta.1", "beta.2"))
#' mcmc_parparser_scalar(c("beta[1]", "beta[2]"))
mcmc_parparser_scalar <- function(x) {
  ret <- mapply(function(pararray, index)
                McmcFlatpar(pararray=pararray, index=index),
         x, 1, SIMPLIFY=FALSE)
  names(ret) <- x
  McmcFlatparList(ret)
}

#' @rdname mcmc_parparsers
#' @aliases mcmc_parparsers_stan
#' @export
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

#' @rdname mcmc_parparsers
#' @aliases mcmc_parparsers_bugs
#' @export
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

#' @rdname mcmc_parparsers
#' @aliases mcmc_parparsers_guess
#' @export
mcmc_parparser_guess <- function(x) {
  if (valid_mcmc_parnames(x, "stan")) {
    mcmc_parparser_stan(x)
  } else if (valid_mcmc_parnames(x, "bugs")) {
    mcmc_parparser_bugs(x)
  } else {
    mcmc_parparser_scalar(x)
  }
}


#' Create McmcPararrayList from McmcFlatparList
#'
#' @param x \linkS4class{McmcFlatparList}
#' @return Object of class \linkS4class{McmcPararrayList}
#' @keywords internal
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
#' @param parser \code{function} parse \code{x} into \linkS4class{McmcFlatparList}.
#' @return Object of class \linkS4class{McmcParameters}
#' @examples
#' mcmc_parse_parnames(c("beta[1]", "beta[2]"))
#' mcmc_parse_parnames(c("beta.1", "beta.2"), mcmc_parparser_stan)
#' @export
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
#' @examples
#' valid_mcmc_parnames("beta[1]")
#' valid_mcmc_parnames("beta[1]", "stan")
#' valid_mcmc_parnames("beta.1", "stan")
valid_mcmc_parnames <- function(x, style="bugs") {
  if (style == "bugs") {
    all(sapply(str_match_all(x, "^[A-Za-z.][A-Za-z.0-9]*(\\[\\d(,\\d)*\\])?$"),
               length))
  } else if (style == "stan") {
    all(sapply(str_match_all(x, "^[A-Za-z][A-Za-z0-9_]*(\\.\\d)*$"), length))
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
## mcmc_create_parnames_stan <- function(x, dim) {
##   if (identical(as.integer(dim), 1L)) {
##     x
##   } else {
##     idxstr <- apply(expand_grid_dim(dim), 1, paste, collapse=".")
##     paste(x, idxstr, sep=".")
##   }
## }

## mcmc_create_parnames_stan_idx <- function(x, idx, dim) {
##   if (identical(as.integer(dim), 1L)) {
##     x
##   } else {
##     paste(x, paste(idx, collapse="."), sep=".")
##   }
## }

## #' Create MCMC parnames
## #'
## #' @rdname mcmc_create_parnames
## #' @aliases mcmc_create_parnames_bugs
## mcmc_create_parnames_bugs <- function(x, dim) {
##   if (identical(as.integer(dim), 1L)) {
##     x
##   } else {
##     idxstr <- apply(expand_grid_dim(dim), 1, paste, collapse=",")
##     paste0(x, "[", idxstr, "]")
##   }
## }

## mcmc_create_parnames_bugs_idx <- function(x, idx) {
##   if (identical(as.integer(dim), 1L)) {
##     x
##   } else {
##     paste0(x, "[", paste(idx, collapse=","), "]")
##   }
## }

## ## bugs_to_stan_parnames <- function(x) {
## ##   gsub("]", "", gsub("[\\[,]", ".", x))
## ## }

## ## stan_to_bugs_parnames <- function(x) {
## ##   y <- str_split_fixed(x, fixed("."), 2)
## ##   paste0(y[ ,1], "[", gsub("\\.", ",", y[ , 2]), "]")
## ## }

