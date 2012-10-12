context("mcmc.list class and methods")

data(line, package="mcmc4")

test_that("mcmc.list class definition", {
    ## Check that class is exported
    expect_true(isClass("mcmc.list"))

    ## Normal initialization
    ## Purposefully use chains of different lengths
    x <- lapply(seq(20,24,by=2),
                function(i) mcmc(matrix(rnorm(i), ncol=2)))
    expect_that(new("mcmc.list", x), is_a("mcmc.list"))

    ## Error if non-mcmc objects in list
    expect_error(new("mcmc.list", list(1, "a", 1:5)))
})

test_that("method rbind2 for mcmc works", {
    linematrix <- rbind2(line)
    expect_is(linematrix, "matrix")
    expect_equal(ncol(linematrix), ncol(line[[1]]))
    expect_equal(nrow(linematrix), sum(sapply(line, nrow)))
})


test_that("as mcmc.list -> matrix works", {
    expect_identical(rbind2(line), as(line, "matrix"))
})
test_that("as mcmc.list -> mcmc works", {
    linemcmc <- as(line, "mcmc")
})


