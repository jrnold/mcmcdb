##' @export
setGeneric("str")

## default str on an McmcLong object would take minutes
setMethod("str", "McmcLong", function(object) show(object))
