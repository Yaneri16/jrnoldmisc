context("select_col")

test_that("select_col works as expected", {
  expect_equal(select_col(tibble(a = 1:3, b = 4:6), a), 1:3)
})

test_that("select_col works as expected", {
  expect_equal(select_col_(tibble(a = 1:3, b = 4:6), "a"), 1:3)
})