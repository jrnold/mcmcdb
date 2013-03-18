#' @include package.R
NULL

## subclasses of matrix that restrict to a given type
NumericMatrix <- setClass("NumericMatrix", "matrix")

setValidity("NumericMatrix",
            function(object) {
              if (!is.numeric(object)) {
                return("Object is not numeric.")
              }
              TRUE
            })

IntegerMatrix <- setClass("IntegerMatrix", "matrix")

setValidity("IntegerMatrix",
            function(object) {
              if (!is.integer(object)) {
                return("Object is not integer.")
              }
              TRUE
            })
