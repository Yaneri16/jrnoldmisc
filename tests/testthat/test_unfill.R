context("text_unfill")

x <- c("a", "a", "a", "b", "b", "c", "d", "d", "a", "a")

test_that("unfill works as expected", {
  expect_equal(unfill(x), c("a", NA, NA, "b", NA, "c", "d", NA, "a", NA))
})

test_that("unfill with last value works as expected", {
  expect_equal(unfill(x, "last"), c(NA, NA, "a", NA, "b", "c", NA, "d", NA, "a"))
})

test_that("unfill with first value works as expected", {
  expect_equal(unfill(x, "middle"), c(NA, "a", NA, "b", NA, "c", "d", NA, "a", NA))
})

test_that("unfill with value argument works as expected", {
  expect_equal(unfill(x, value = "..."), c("a", "...", "...", "b", "...", "c", "d", "...", "a", "..."))
})