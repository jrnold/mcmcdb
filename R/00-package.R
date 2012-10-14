#' Some S4 classes for MCMC sample data objects
#'
#' S4 wrappers of the classes to store mcmc objects in \pkg{coda},
#' as well as a few new classes.
#'
#' @name mcmc4
#' @docType package
#' @import plyr
#' @import reshape2
#' @import stringr
#' @import DataFramePlus
#' @importMethodsFrom stats4 coef vcov
NULL

## Classes
## setClassUnion("DataFrameOrNull", c("data.frame", "NULL"))

## Missing Coercions
setAs("character", "factor", function(from, to) as.factor(from))

