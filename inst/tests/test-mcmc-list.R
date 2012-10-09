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

test_that("method mean for signature mcmc.list works", {
    expect_that(length(mean(line)), equals(ncol(line[[1]])))
})
test_that("method median for signature mcmc.list works", {
    expect_that(length(median(line)), equals(ncol(line[[1]])))
})
test_that("method quantile for signature mcmc.list works", {
    expect_that(dim(quantile(line)), equals(c(5, ncol(line[[1]]))))
})
test_that("method vcov for signature mcmc.list works", {
    expect_that(dim(vcov(line)), equals(rep(ncol(line[[1]]), 2)))
})
test_that("method coef for signature mcmc.list works", {
    expect_that(coef(line), equals(mean(line)))
    expect_that(coef(line, "median"), is_equivalent_to(median(line)))
})

test_that("method rbind2 for mcmc works", {
    linematrix <- rbind2(line)
    expect_is(linematrix, "matrix")
    expect_equal(ncol(linematrix), ncol(line[[1]]))
    expect_equal(nrow(linematrix), sum(sapply(line, nrow)))
})

test_that("melt works", {
    foo <- melt(line)
    expect_is(foo, "data.frame")
    expect_equal(colnames(foo), c("parameter", "chain", "iteration", "value"))
    expect_equal(nrow(foo), 1200)
    expect_equal(rownames(foo)[1], "alpha.1.1")
    expect_equal(rownames(foo)[1200], "sigma.2.200")
})

test_that("as mcmc.list -> matrix works", {
    expect_identical(rbind2(line), as(line, "matrix"))
})
test_that("as mcmc.list -> mcmc works", {
    linemcmc <- as(line, "mcmc")
})


