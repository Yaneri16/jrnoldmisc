context("complete_cases")

x <- tibble(a = c(1, 2, NA, NA),
            b = c(5, NA, 6, NA),
            c = rep(NA, 4),
            d = letters[1:4])

test_that("complete_cases works with all rows having missing", {
  expect_equivalent(complete_cases(x),
                    tibble(a = numeric(),
                           b = numeric(),
                           c = logical(),
                           d = character()))
})

test_that("complete_cases works with some rows having all missing values", {
  expect_equivalent(complete_cases(select(x, a, b, d)),
                    select(x, a, b, d)[1, ])
})

test_that("complete_cases works with selecting rows", {
  expect_equivalent(complete_cases(x, a, d),
                    x[c(1, 2), ])
})

test_that("complete_cases_ works", {
  expect_equivalent(complete_cases_(x, ~a, "d"),
                    x[c(1, 2), ])
})
