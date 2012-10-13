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
#' @importMethodsFrom stats4 coef vcov
NULL

## Classes
## setClassUnion("DataFrameOrNull", c("data.frame", "NULL"))

## Missing Coercions
setAs("character", "factor", function(from, to) as.factor(from))

## Check whether columns could be a key to a data.frame
## 
## @param x \code{data.frame} to check
## @param i \code{character} or \code{integer} Column names
## @return \code{logical}. \code{TRUE} if no duplicated values
## of \code{i} in \code{x}.
is_key <- function(x, i) {
    !any(duplicated(x[ , i]))
}

## Check column names and classes of a \code{data.frame}
##
## @param object \code{data.frame} to be validated.
## @param columns Named \code{character} vector. Names are required
## @param exclusive \code{logical} If \code{TRUE}, then \code{object}
## cannot contain any columns other than those in \code{columns}
## columns in \code{x}, values are the classes of those columns.
## @returns If valid, then \code{TRUE}, else \code{character} with
## an error message.
validate_data_frame <- function(object, columns, exclusive=FALSE, keys=c()) {
    # error checking functions
    if (any(! keys %in% names(columns))) {
        stop("Keys must be in columns")
    }
    # actual 
    for (i in names(columns)) {
        if (! i %in% colnames(object)) {
            return(sprintf("column %s not in 'object'", i))
        }
        if (!is(object[[i]], columns[[i]])) {
            return(sprintf("column %s does not have class %s",
                           i, columns[[i]]))
        }
    }
    if (exclusive) {
        othercols <- setdiff(colnames(object), names(columns))
        if (length(othercols)) {
            return(sprintf("invalid columns: %s",
                           paste(sQuote(othercols), collapse=", ")))
        }
    }
    if (length(keys)) {
        if (! is_key(object, keys)) {
            return(sprintf("columns %s are not unique",
                           paste(sQuote(keys), collapse=", ")))
        }
    }
    TRUE
}

subclass_data_frame <- function(class, columns, exclusive=FALSE, keys=character()) {
    setClass(class, "data.frame")

    setValidity(class,
                function(object) {
                    validate_data_frame(object, columns,
                                        exclusive=exclusive,
                                        keys=keys)
                })
    
    setMethod("initialize", class,
              function(.Object, x) {
                  ## Drop any bad columns if exclusive
                  if (exclusive) {
                      coltouse <- intersect(names(x), names(columns))
                      x <- x[ , coltouse]
                  }
                  ## Coerce column types
                  for (i in names(columns)) {
                      x[[i]] <- as(x[[i]], columns[i])
                  }
                  .Object <- callNextMethod(.Object, x)
                  validObject(.Object)
                  .Object
              })

    setAs("data.frame", class,
          function(from, to) new(class, from))
    
    .f <- function(x) {
        new(class, x)
    }
    
    invisible(.f)
}


setClass("DataFramePlus", contains="data.frame",
         representation(required="character",
                        classes="character",
                        exclusive="logical",
                        keys="character"),
         prototype(data.frame(),
                   required = character(),
                   classes=character(),
                   exclusive=FALSE,
                   keys=character()))

setValidity("DataFramePlus",
            function(object) {
                if(length(object@required) != length(object@classes)) {
                    return("length(object@required) != length(object@classes)")
                }
                validate_data_frame(object, setNames(object@classes, object@required),
                                    exclusive=object@exclusive, keys = object@keys)
            })

setMethod("initialize", "DataFramePlus",
          function(.Object, x, required=character(), classes=character(), exclusive=FALSE, keys=character()) {
              if(length(required) != length(classes)) {
                  return("length(object@required) != length(object@classes)")
              }
              ## Drop any bad columns if exclusive
              if (exclusive) {
                  coltouse <- intersect(names(x), required)
                  x <- as.data.frame(x)[ , coltouse, drop=FALSE]
              }
              ## Coerce column types
              for (i in seq_along(required)) {
                  nm <- required[i]
                  cls <- classes[i]
                  x[[nm]] <- as(x[[nm]], cls)
              }
              .Object <- callNextMethod(.Object, x)
              .Object@required <- required
              .Object@classes <- classes
              .Object@keys <- keys
              .Object@exclusive <- exclusive
              validObject(.Object)
              .Object
          })


printvec <- function(x) paste(sQuote(x, collapse=', '))

subclass_data_frame_plus <- function(class, required=character(),
                                     classes=character(), exclusive=FALSE,
                                     keys=character()) {

    .data <- list()
    for (i in seq_along(required)) {
        .data[[required[i]]] <- new(classes[i])
    }
    .data <- data.frame(.data)
    
    setClass(class, contains="DataFramePlus",
             prototype=prototype(x=.data, required=required,
                       classes=classes, exclusive=exclusive,
                       keys=keys))

    setValidity(class,
                function(object) {
                    if (!setequal(object@required, required)) {
                        return(sprintf("object@required: %s != %s",
                                       deparse(object@required), deparse(required)))
                    }
                    if (!setequal(object@classes, classes)) {
                        return(sprintf("object@classes: %s != %s",
                                       deparse(object@classes), deparse(classes)))
                    }
                    if (object@exclusive != exclusive) {
                        return(sprintf("exclusive: != %s", exclusive))
                    }
                    if (!setequal(object@keys, keys)) {
                        return(sprintf("object@keys: %s != %s",
                                       deparse(object@keys), deparse(keys)))
                    }
                    validObject(as(object, "DataFramePlus"))
                    TRUE
                })
    
    setMethod("initialize", class,
              function(.Object, x) {
                  if (missing(x)) {
                      x <- .data
                  }
                  .Object <- callNextMethod(.Object, x=x, required=required,
                                            classes=classes,
                                            exclusive=exclusive, keys=keys)
                  .Object
              })

    setAs("data.frame", class,
          function(from, to) new(class, from))
    
    .f <- function(x) {
        new(class, x)
    }
    invisible(.f)
}
