context("summary.mcmc tests")

foo <- mcmc(matrix(rnorm(30), 10, 3))
foo_summary <- summary(foo)

test_that("summary.mcmc class exists", {
    expect_true(isClass("summary.mcmc"))
    expect_true(extends("summary.mcmc", "oldClass"))
})

test_that("summary.mcmc coerces to matrix", {
    x <- as(foo_summary, "matrix")
    xcolnames <- c("Mean", "SD", "Naive SE", "Time-series SE", "2.5%", "25%",
                   "50%", "75%", "97.5%")
    expect_is(x, "matrix")
    expect_equal(dim(x), c(3, length(xcolnames)))
    expect_equal(colnames(x), xcolnames)
})

test_that("summary.mcmc coerces to data.frame", {
    x <- as(foo_summary, "data.frame")
    xcolnames <- c("Mean", "SD", "Naive.SE", "Time.series.SE", "X2.5.", "X25.",
                   "X50.", "X75.", "X97.5.", "variable")
    expect_is(x, "data.frame")
    expect_equal(dim(x), c(3, 10))
    expect_equal(colnames(x), xcolnames)
    expect_equal(x$variable, paste0("var", 1:3))
})

