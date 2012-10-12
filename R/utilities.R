## Missing Coercions
setAs("character", "factor", function(from, to) as.factor(from))

## Select first observation
first <- function(x) x[1]

## Select last observation
last <- function(x) x[length(x)]

## Paste combination
##
## Combines
expand.paste <- function (..., sep = "") {
    apply(expand.grid(...), 1, paste, collapse = sep)
}

## Arrays of zeros
##
## @param dim \code{numeric} Dimension of output.
## @returns \code{numeric} if \code{length(dim) == 1} else
## \code{array}.
zeros <- function(dim) {
    if (length(dim) <= 1) {
        numeric(dim)
    } else {
        array(0, dim)
    }
}

str_all_match <- function(string, pattern) {
    !any(is.na(str_match(string, pattern)[ , 1]))
}

## Strip some attributes added by plyr
strip_plyr_attr <- function(x) {
    for (i in c("split_type", "split_labels")) {
        attr(x, i) <- NULL
    }
    x
}

## zeros_int <- function(dim) {
##     if (length(dim) <= 1) {
##         integer(dim)
##     } else {
##         array(0L, dim)
##     }
## }

## zeros_logical <- function(dim) {
##     if (length(dim) <= 1) {
##         logical(dim)
##     } else {
##         array(FALSE, dim)
##     }
## }

## Check column names and classes of a \code{data.frame}
##
## @param object \code{data.frame} to be validated.
## @param columns Named \code{character} vector. Names are required
## @param exclusive \code{logical} If \code{TRUE}, then \code{object}
## cannot contain any columns other than those in \code{columns}
## columns in \code{x}, values are the classes of those columns.
## @returns If valid, then \code{TRUE}, else \code{character} with
## an error message.
validate_data_frame <- function(object, columns, exclusive=FALSE) {
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
            return("invalid columns: %s",
                   paste(sQuote(othercols), sep=", "))
        }
    }
    TRUE
}


