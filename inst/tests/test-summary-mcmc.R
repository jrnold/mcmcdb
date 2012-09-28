context("summary.mcmc tests")

data("line", package="coda")
line_summary <- summary(line)

test_that("summary.mcmc class exists", {
    expect_true(isClass("summary.mcmc"))
    expect_true(extends("summary.mcmc", "oldClass"))
})

test_that("summary.mcmc coerces to a matrix", {
    x <- as(line_summary, "matrix")
    xcolnames <- c("Mean", "SD", "Naive SE", "Time-series SE", "2.5%", "25%",
                   "50%", "75%", "97.5%")
    expect_is(x, "matrix")
    expect_equal(dim(x), c(3, length(xcolnames)))
    expect_equal(colnames(x), xcolnames)
})

test_that("summary.mcmc coerces data.frame", {
    x <- as(line_summary, "data.frame")
    xcolnames <- c("Mean", "SD", "Naive.SE", "Time.series.SE", "X2.5.", "X25.",
                   "X50.", "X75.", "X97.5.", "variable")
    expect_is(x, "data.frame")
    expect_equal(dim(x), c(3, 10))
    expect_equal(colnames(x), xcolnames)
    expect_equal(x$variable, c("alpha", "beta", "sigma"))
})


