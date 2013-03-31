#' @include package.R
#' @exportClass McmcdbSamples
#' @exportClass McmcdbChains
#' @exportClass McmcdbIters
#' @exportClass McmcdbFlatparChains
#' @exportClass McmcdbFlatpars
#' @export McmcdbSamples
#' @export McmcdbChains
#' @export McmcdbIters
#' @export McmcdbFlatparChains
#' @export McmcdbFlatpars
NULL

#' Miscellaneous classes
#'
#' Various classes used internally in this package.
#' These are all subclasses of \code{\linkS4class{CheckedFrame}},
#' i.e. \code{data.frame} objects with required
#' columns.
#'
#' @name misc-classes
#' @rdname misc-classes
#' @aliases McmcdbSamples-class
#' @aliases McmcdbSamples
#' @aliases McmcdbChains-class
#' @aliases McmcdbChains
#' @aliases McmcdbIters-class
#' @aliases McmcdbIters
#' @aliases McmcdbFlatparChains-class
#' @aliases McmcdbFlatparChains
#' @aliases McmcdbFlatpars-class
#' @aliases McmcdbFlatpars
#' @keywords classes
#' @docType class
#' @examples
#' showClass("McmcdbSamples")
#' showClass("McmcdbChains")
#' showClass("McmcdbIters")
#' showClass("McmcdbFlatparChains")
#' showClass("McmcdbFlatpars")
NULL

mcmcdb_samples_checks <-
  TableChecks(ColumnCheckList(flatpar = ColumnChecks("factor"),
                              chain_id = ColumnChecks("integer"),
                              iter = ColumnChecks("integer"),
                              val = ColumnChecks("numeric")))

McmcdbSamples <- 
  checked_frame_class("McmcdbSamples",
                      mcmcdb_samples_checks)

#######

McmcdbChains <- 
  checked_frame_class("McmcdbChains",
                      columns =
                      ColumnCheckList(chain_id = ColumnChecks("integer", unique=TRUE)))

#######

mcmcdb_iters_checks <-
  TableChecks(ColumnCheckList(chain_id = ColumnChecks("integer"),
                              iter = ColumnChecks("integer")),
              constraints =
              FunctionList(function(x) {
                ! any(duplicated(x[ , c("chain_id", "iter")]))
              }))

McmcdbIters <- 
  checked_frame_class("McmcdbIters", mcmcdb_iters_checks)

#######

McmcdbFlatparChains <- 
  checked_frame_class("McmcdbFlatparChains",
                      columns =
                      ColumnCheckList(flatpar = ColumnChecks("factor"),
                                      chain_id = ColumnChecks("integer"),
                                      init = ColumnChecks("numeric")),
                      constraints =
                      FunctionList(function(x) {
                        ! any(duplicated(x[ , c("flatpar", "chain_id")]))
                      }))


#######

index_regex <- function(x) all(str_detect(x, "^\\d+(,\\d)*$"))

mcmcdb_flatpars_columns <-
  ColumnCheckList(idx =
                  ColumnChecks("character",
                               constraints =
                               FunctionList(index_regex=index_regex)),
                  flatpar = ColumnChecks("character", unique=TRUE),
                  pararray = ColumnChecks("character"),
                  scalar = ColumnChecks("logical"))

McmcdbFlatpars <-
  checked_frame_class("McmcdbFlatpars",
                      columns = mcmcdb_flatpars_columns)

                         
