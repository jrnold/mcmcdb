##' Some S4 classes for MCMC sample data objects
##'
##' S4 wrappers of the classes to store mcmc objects in \pkg{coda},
##' as well as a few new classes.
##'
##' @name mcmcdb
##' @docType package
##' @import plyr
##' @import stringr
##' @import reshape2
##' @import checker
NULL

VERSION <- "0.3-0"

setOldClass("mcmc")
setOldClass("mcmc.list")
