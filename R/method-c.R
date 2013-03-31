#' @include package.R
#' @include class-McmcdbWide.R
#' @include method-rbind2.R
#' @exportMethod c
NULL

#' @rdname c-methods
#' @name c-methods
#' @title Methods for function \code{c}
#'
#' @param x First object
#' @param ... Other objects to concatenate to \code{x}.
#' @return An object of the same class as \code{x}.
NULL

#' @rdname c-methods
#' @aliases c,McmcdbWide-method
setMethod("c", c(x = "McmcdbWide"), 
          function(x, ...) Reduce(rbind2, list(x, ...)))
