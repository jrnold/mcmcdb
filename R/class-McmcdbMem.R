#' @include package.R
#' @include utilities.R
#' @include class-misc.R
#' @include class-matrix.R
#' @include class-Mcmcdb.R
#' @include class-McmcdbParameters.R
#' @include method-McmcdbParameters.R
#' @exportClass McmcdbMem
NULL

#' @name McmcdbMem-class
#' @rdname McmcdbMem-class
#' @docType class
#' @aliases McmcdbMem-class
#' 
#' @title MCMC Samples Virtual Class
#'
#' @description Class for storing MCMC samples in-memory.
#'
#' @family Mcmcdb classes
setClass("McmcdbMem",
         contains = "Mcmcdb",
         representation(parameters="McmcdbParameters",
                        chains="McmcdbChains", # chain_id
                        iters="McmcdbIters", # chain_id, iter
                        flatpar_chains="ANY", # parname, chain_id
                        metadata="list",
                        version="character",
                        parinit="numeric",
                        model_data="namedList"),
         prototype(parameters = McmcdbParameters(),
                   chains = McmcdbChains(chain_id = integer()),
                   iters = McmcdbIters(chain_id = integer(),
                     iter = integer()),
                   metadata = list(),
                   version = as.character(packageVersion("mcmcdb")),
                   parinit = numeric(),
                   flatpar_chains = NULL,
                   model_data = nlist()))

