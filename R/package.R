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
##' @import stanmisc
NULL

setOldClass("mcmc")
setOldClass("mcmc.list")
setOldClass("mcarray")
