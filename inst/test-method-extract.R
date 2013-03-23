context("method-extract")
source("data-McmcdbWide.R")

###########

test_that("[,McmcdbWide,character,missing,missing,missing", {
  expect_equal(test_wide2["beta.1"], 1:4)
})

test_that("[,McmcdbWide,character,integer,missing,missing", {
  expect_equal(test_wide2["beta.1", 2], 3:4)
})

test_that("[,McmcdbWide,character,integer,missing,missing", {
  expect_equal(test_wide2["beta.1", 2, 2], 4)
})

test_that("[,McmcdbWide,character,integer,integer,missing", {
  expect_equal(test_wide2["beta.1", 2, 2], 4)
})

## 

test_that("[,McmcdbWide,character,missing,missing drop=FALSE", {
  expected <- McmcdbSamples(data.frame(flatpar="beta.1",
                                       chain_id = rep(1:2, each=2),
                                       iter = rep(1:2, 2),
                                       val = 1:4))
  expect_equal(test_wide2["beta.1", drop=FALSE], expected)
})

test_that("[,McmcdbWide,character,missing,integer drop=FALSE", {
  expected <- McmcdbSamples(data.frame(flatpar="beta.2",
                                       chain_id = 1:2,
                                       iter = 2L,
                                       val = c(6L, 8L)))
  expect_equal(test_wide2["beta.2", , 2, drop=FALSE], expected)
})

test_that("[,McmcdbWide,character,missing,missing drop=FALSE", {
  expected <- McmcdbSamples(data.frame(flatpar= rep(c("beta.1", "beta.2"), each = 2),
                                       chain_id = 1L,
                                       iter = rep(1:2, rep=2),
                                       val = as.integer(c(1, 2, 5, 6))))
  expect_equal(test_wide2[ , 1, , drop=FALSE], expected)
})

test_that("[,McmcdbWide works with multiple indices", {
  foo <- test_wide2[c("beta.1", "beta.2"), 1:2, 1:2]
  expect_equal(foo, 1:8)
})

##########################################

test_that("[[,McmcdbWide,character,missing,missing drop=TRUE", {
  expected <- t(array(1:8, c(4, 2)))
  expect_equal(test_wide2[["beta"]], expected)
})

test_that("[[,McmcdbWide,character,missing,missing drop=FALSE", {
  foo <- test_wide2[["beta", drop=FALSE]]
  expected <-
    McmcdbSamples(data.frame(flatpar= rep(c("beta.1", "beta.2"), each = 4),
                             chain_id = rep(rep(1:2, each=2), 2),
                             iter = rep(1:2, 4),
                             val = 1:8))
  expect_equal(foo, expected)
})

test_that("[[,McmcdbWide,character,missing,integer drop=TRUE", {
  foo <- test_wide2[["beta", , 2]]
  expected <- array(as.integer(c(2, 6, 4, 8)), c(2, 2))
  expect_equal(test_wide2[["beta", , 2]], expected)
})

test_that("[[,McmcdbWide,character,missing,integer drop=FALSE", {
  foo <- test_wide2[["beta", , 2, drop=FALSE]]
  expected <- McmcdbSamples(data.frame(flatpar= rep(c("beta.1", "beta.2"), each = 2),
                                       chain_id = rep(1:2, 2),
                                       iter = 2L,
                                       val = as.integer(c(2, 4, 6, 8))))
  expect_equal(foo, expected)
})

test_that("[[,McmcdbWide works with multiple indices", {
  foo <- test_wide2[["beta", 1:2, 1:2]]
  expect_equal(dim(foo), c(2L, 4L))
})


###############################################

test_that("$,McmcdbWide works", {
  foo <- test_wide2$beta
  expected <- t(array(1:8, c(4, 2)))
  expect_equal(foo, expected)
})


