context("fct_remove")

test_that("fct_remove can drop levels", {
  f1 <- factor(c("a", "b", "c", "d"))
  f2 <- factor(c(NA, "b", NA, "d"))
  expect_equal(fct_remove(f1, c("a", "c")), f2)
})

test_that("fct_remove does nothing if dropped level not in factor levels", {
  f1 <- factor(c("a", "b", "c", "d"))
  expect_equal(fct_remove(f1, c("e")), f1)
})
