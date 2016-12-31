context("select helpers")

test_that("glob() works as expected", {
  expect_equal(names(dplyr::select(iris, glob("Sepal*"))),
               c("Sepal.Length", "Sepal.Width"))
})