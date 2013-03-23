#' @include package.R
#' @include utilities.R
NULL

#' @name line_samples
#' @rdname line_samples
#' 
#' @title Line MCMC samples
#'
#' @description A \code{"McmcdbWide"} object used for examples in this package.
#'
#' @format An object of class \code{"McmcdbWide"} with
#' 2 parameter arrays, \code{beta} (length 2) and \code{tau} (length 1),
#' and 2 chains of 100 samples each.
#'
#' @section Model:
#'
#' The data was generated from this JAGS model,
#'
#' \Sexpr[results=rd,stage=build]{data(line_samples); mcmcdb:::print_rd_model_code(line_samples)}
#'
#' @docType data
#' @examples
#' data(line_samples)
#' show(line_samples)
#' mcmcdb_flatpars(line_samples)
#' mcmcdb_mcpar(line_samples)
NULL
