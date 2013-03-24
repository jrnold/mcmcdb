#' @include package.R
#' @include class-Mcmcdb.R
#' @include method-extract-bracket2.R
#' @exportMethod $
NULL

#' @name $-methods
#' @rdname extract-dollar-methods
#' @docType methods
#' @title Methods for function \code{$}
#'
#' @param x An object.
#' @param name \code{character} Paramter array name.
#'
#' @seealso See \code{\linkS4class{McmcdbWide}} for examples.
NULL

`$.Mcmcdb` <- function(x, name) {
  x[[name]]
}

#' @rdname extract-dollar-methods
#' @aliases $,Mcmcdb-method
#' @family McmcdbWide methods
setMethod("$", c(x="Mcmcdb"), `$.Mcmcdb`)
