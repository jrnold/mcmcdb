#' @include utilities.R
#' @include class-McmcdbParameters.R
#' @export mcmc_parparser_bugs
#' @export mcmc_parparser_stan
#' @export mcmc_parparser_scalar
#' @export mcmc_parparser_guess
#' @export mcmc_parse_parnames
NULL

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
#' @return Object of class \code{McmcdbParameters}
#'
#' @rdname mcmc_parparsers
#' @aliases mcmc_parparsers_scalar
#' @seealso \code{\link{mcmc_parse_parnames}} which takes these functions an argument.
#' @examples
#' mcmc_parparser_bugs(c("beta[1]", "beta[2]"))
#' mcmc_parparser_stan(c("beta.1", "beta.2"))
#' mcmc_parparser_scalar(c("beta[1]", "beta[2]"))
mcmc_parparser_scalar <- function(x) {
  ret <- mapply(function(pararray, index)
                McmcdbFlatpar(pararray=pararray, index=index),
         x, 1, SIMPLIFY=FALSE)
  names(ret) <- x
  McmcdbFlatparList(ret)
}

#' @rdname mcmc_parparsers
#' @aliases mcmc_parparsers_stan
mcmc_parparser_stan <- function(x) {
  x2 <- str_split_fixed(x, fixed("."), 2)
  indices <- llply(str_split(x2[ , 2], fixed(".")),
                   function(x) ifelse(x == "", 1, as.integer(x)))
  ret <- mapply(function(pararray, index)
                McmcdbFlatpar(pararray=pararray, index=index),
                x2[ , 1], indices, SIMPLIFY=FALSE)
  names(ret) <- x
  McmcdbFlatparList(ret)
}

#' @rdname mcmc_parparsers
#' @aliases mcmc_parparsers_bugs
mcmc_parparser_bugs <- function(x) {
  x2 <- str_match(x, "([^\\[]+)(\\[([0-9,]+)\\])?")[ , c(2, 4)]
  indices <- llply(str_split(x2[ , 2], fixed(",")),
                   function(x) ifelse(x == "", 1, as.integer(x)))
  ret <- mapply(function(pararray, index)
                McmcdbFlatpar(pararray=pararray, index=index),
                x2[ , 1], indices, SIMPLIFY=FALSE)
  names(ret) <- x
  McmcdbFlatparList(ret)
}

#' @rdname mcmc_parparsers
#' @aliases mcmc_parparsers_guess
mcmc_parparser_guess <- function(x) {
  if (all(valid_mcmc_parnames(x, "stan"))) {
    mcmc_parparser_stan(x)
  } else if (all(valid_mcmc_parnames(x, "bugs"))) {
    mcmc_parparser_bugs(x)
  } else {
    mcmc_parparser_scalar(x)
  }
}


#' Create McmcdbPararrayList from McmcdbFlatparList
#'
#' @param x \linkS4class{McmcdbFlatparList}
#' @return Object of class \linkS4class{McmcdbPararrayList}
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
  ret <- McmcdbPararrayList(mapply(function(x, y) {
    McmcdbPararray(dim = x, flatpars = y)
  }, pardims,  flatpars))
  names(ret) <- pararrays
  ret
}

#' Create McmcdbParameter object from MCMC parameter names
#'
#' @param x \code{character} vector of parameter names
#' @param parser \code{function} parse \code{x} into \linkS4class{McmcdbFlatparList}.
#' @return Object of class \linkS4class{McmcdbParameters}
#' @examples
#' mcmc_parse_parnames(c("beta[1]", "beta[2]"))
#' mcmc_parse_parnames(c("beta.1", "beta.2"), mcmc_parparser_stan)
mcmc_parse_parnames <- function(x, parser = mcmc_parparser_bugs) {
  flatpars <- parser(x)
  pararrays <- create_pararrays(flatpars)
  McmcdbParameters(flatpars = flatpars, pararrays = pararrays)
}
