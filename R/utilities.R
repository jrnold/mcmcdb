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


