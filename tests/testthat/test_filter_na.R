context("filter_na")

x <- tibble(a = c(1, 2, NA, NA),
            b = c(5, NA, 6, NA),
            c = rep(NA, 4),
            d = letters[1:4])

test_that("filter_na works with all rows having missing values", {
  expect_equivalent(filter_na(x),
                    tibble(a = numeric(),
                           b = numeric(),
                           c = logical(),
                           d = character()))
})

test_that("fitler_na works with some rows having missing values", {
  expect_equivalent(filter_na(x, a, b, d),
                    x[1, ])
})

test_that("filter_na works with .all = TRUE", {
  expect_equivalent(filter_na(x, a, b, c, .all = TRUE),
                    x[1:3, ])
})

test_that("filter_na works with .finite = TRUE and Inf values", {
  x <- tibble(a = c(1, 2, NA, NA, Inf, -Inf, NaN),
              b = c(5, NA, 6, NA, 1, 1, 1))
  expect_equivalent(filter_na(x, a, b, .finite = TRUE),
                    x[1, ])
})

test_that("filter_na works with .finite = FALSE and Inf values", {
  x <- tibble(a = c(1, 2, NA, NA, Inf, -Inf, NaN),
              b = c(5, NA, 6, NA, 1, 1, 1))
  expect_equivalent(filter_na(x, a, b, .finite = FALSE),
                    x[c(1, 5, 6), ])
})

test_that("filter_na_ works", {
  expect_equivalent(complete_cases_(x, ~a, "d"),
                    x[1:2, ])
})