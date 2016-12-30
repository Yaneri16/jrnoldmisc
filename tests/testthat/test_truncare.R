context("truncare and censor")

x <- c(-2:2, -Inf, Inf, NaN, NA)

test_that("truncare works with defaults", {
  expect_equal(truncare(x), x)
})

test_that("truncare left works", {
  expect_true(all.equal(truncare(x, left = -1),
                        c(NA, -1:2, NA, Inf, NaN, NA)))
})

test_that("truncare right works", {
  expect_true(all.equal(truncare(x, right = 1),
                        c(-2:1, NA, -Inf, NA, NaN, NA)))
})

test_that("truncare left with newleft works", {
  expect_true(all.equal(truncare(x, left = -1, newleft = -5),
                        c(-5, -1:2, -5, Inf, NaN, NA)))
})

test_that("truncare right with newright works", {
  expect_true(all.equal(truncare(x, right = 1, newright = 5),
                        c(-2:1, 5, -Inf, 5, NaN, NA)))
})

test_that("truncare leq and geq work", {
  expect_true(all.equal(truncare(x, right = 1, left = -1,
                                 geq = FALSE, leq = FALSE),
                        c(NA, -1, 0, 1, NA, NA, NA, NaN, NA)))
})
