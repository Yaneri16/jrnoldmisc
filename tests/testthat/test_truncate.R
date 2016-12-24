context("truncate and censor")

x <- c(-2:2, -Inf, Inf, NaN, NA)

test_that("truncate works with defaults", {
  expect_equal(truncate(x), x)
})

test_that("truncate left works", {
  expect_true(all.equal(truncate(x, left = -1),
                        c(NA, -1:2, NA, Inf, NaN, NA)))
})

test_that("truncate right works", {
  expect_true(all.equal(truncate(x, right = 1),
                        c(-2:1, NA, -Inf, NA, NaN, NA)))
})

test_that("truncate left with newleft works", {
  expect_true(all.equal(truncate(x, left = -1, newleft = -5),
                        c(-5, -1:2, -5, Inf, NaN, NA)))
})

test_that("truncate right with newright works", {
  expect_true(all.equal(truncate(x, right = 1, newright = 5),
                        c(-2:1, 5, -Inf, 5, NaN, NA)))
})

test_that("truncate leq and geq work", {
  expect_true(all.equal(truncate(x, right = 1, left = -1,
                                 geq = FALSE, leq = FALSE),
                        c(NA, -1, 0, 1, NA, NA, NA, NaN, NA)))
})
