#' @include package.R
#' @include method-McmcdbWide.R
#' @export mcmcdb_wide_from_stan
NULL

#' Create McmdbWide from Stan csv files
#'
#' This returns both the sample values and the metadata in the comments of the file.
#' This function has been tested for the output of Stan 1.2.0.
#'
#' @param file \code{character}. Name of an output file produced by a STAN model.
#' @param init \code{list} or \code{NULL}. List of initial values.
#' @param model_data \code{list} or \code{NULL}. List of model data.
#' @param model_name \code{character} Model name.
#' @param model_code \code{character} Stan model code.
#' 
#' @return An object of class \code{"McmcdbWide"}
mcmcdb_wide_from_stan <- function(file, init=NULL, model_data=NULL, model_name=NULL, model_code=NULL) {
  McmdbWide(read_stan_samples(file), init = init,
            model_data = model_data, model_name = model_name)
}
