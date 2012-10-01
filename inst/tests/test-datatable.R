## library(reshape2)
## library(stringr)
## library(data.table)
## # library(mcmc4)

## setClass("McmcTable",
##          contains="data.table",
##          representation(parameters="list",
##                         template="list"))

## setClass("DataTable2",
##          contains="data.table",
##          representation(foo="list",
##                         bar="list"))

## setClass("DataTable3",
##          contains="data.table")

## foo <- data.table(parameter=rep(c("alpha", "beta"), each=10),
##                   chain=rep(1:2, each=5, 2),
##                   iteration=rep(rep(1:5, 2), 2),
##                   value=rnorm(20),
##                   key=c("parameter", "chain", "iteration"))

## bar <- new("McmcTable", foo)
## baz <- new("McmcTable", foo, parameters=list("foo"), template=list(matrix(0)))
## qux <- new("DataTable2", foo)
## asdf <- new("DataTable3", foo)

## tables()
## summary(foo)
## summary(bar)
## summary(baz)
## summary(qux)
## summary(asdf)


## foo[ "alpha", ]
## bar[ "alpha", ]
## baz[ "alpha", ]
## qux[ "alpha", ]
## asdf["alpha", ]

## foo[ , sum(value)]
## bar[ , sum(value)]
## baz[ , sum(value)]
## qux[ , sum(value)]
## asdf[ , sum(value)]
