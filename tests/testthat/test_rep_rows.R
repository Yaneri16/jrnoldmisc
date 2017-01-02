context("rep_rows")

x <- tibble(a = 1:3, b = 4:6)

test_that("rep_rows works as expected", {
  expect_equal(rep_rows(x, 2), bind_rows(x, x))
})

test_that("rep_rows works as expected", {
  expect_equal(rep_rows(x, each = 2),
               tibble(a = as.integer(c(1, 1, 2, 2, 3, 3)),
                      b = as.integer(c(4, 4, 5, 5, 6, 6))))
})