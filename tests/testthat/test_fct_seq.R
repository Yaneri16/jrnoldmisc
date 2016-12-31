context("fct_seq")

test_that("fct_seq works as expected", {
  x <- factor(c("b", "b", "a", "c", "c", "c"))
  expect_equal(levels(fct_seq(x)), as.character(1:3))
})

test_that("fct_seq works with a pattern", {
  x <- factor(c("b", "b", "a", "c", "c", "c"))
  expect_equal(levels(fct_seq(x, "lvl%d")), paste0("lvl", 1:3))
})

test_that("fct_seq works with a function", {
  x <- factor(c("b", "b", "a", "c", "c", "c"))
  expect_equal(levels(fct_seq(x, function(i) paste0("level_#", i))),
               paste0("level_#", 1:3))
})

test_that("fct_seq throws error a non-function or string", {
  expect_error(levels(fct_seq(letters, 1)),
               regexp = "Expected a function or string")
})

test_that("fct_seq throws error with a character vector of length > 1", {
  expect_error(levels(fct_seq(letters, c("foo%d", "bar"))),
               regexp = "Expected a character vector of length 1")
})