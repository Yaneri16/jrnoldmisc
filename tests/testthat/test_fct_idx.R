context("fct_idx")

test_that("fct_idx works as expected", {
  x <- factor(c("b", "b", "a", "c", "c", "c"))
  expect_equal(levels(fct_idx(x)), as.character(1:3))
})

test_that("fct_idx works with a pattern", {
  x <- factor(c("b", "b", "a", "c", "c", "c"))
  expect_equal(levels(fct_idx(x, "lvl%d")), paste0("lvl", 1:3))
})

test_that("fct_idx works with a function", {
  x <- factor(c("b", "b", "a", "c", "c", "c"))
  expect_equal(levels(fct_idx(x, function(i) paste0("level_#", i))),
               paste0("level_#", 1:3))
})

test_that("fct_idx throws error a non-function or string", {
  expect_error(levels(fct_idx(letters, 1)),
               regexp = ".* is not a string .* is not a function or .* is not TRUE")
})

test_that("fct_idx throws error with a character vector of length > 1", {
  expect_error(levels(fct_idx(letters, c("foo%d", "bar"))),
               regexp = "not a string.*not a function.*is not TRUE")
})