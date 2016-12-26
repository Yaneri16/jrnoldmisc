context("betwixt")

x <- c(-2:2, Inf, -Inf, NaN, NA)

test_that("betwixt works with defaults", {
  expect_equal(betwixt(x),
               c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE))
})

test_that("betwixt arguments left, right work", {
  expect_equal(betwixt(x, left = -1, right = 1),
               c(FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE))
})


test_that("betwixt arguments leq, geq work", {
  expect_equal(betwixt(x, left = -1, right = 1, leq = FALSE, geq = FALSE),
               c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE))
})

test_that("betwixt argument na.rm works", {
  expect_equal(betwixt(x, left = -1, right = 1, na.rm = FALSE),
               c(FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, NA, NA))
})
