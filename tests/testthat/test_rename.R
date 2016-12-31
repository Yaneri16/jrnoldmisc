context("rename")

test_that("rename_seq works as expected", {
  expect_equal(names(rename_seq(tibble(a = 1, b = 1, c = 1))),
               paste0("Var", 1:3))
})

test_that("rename_seq works with a pattern", {
  expect_equal(names(rename_seq(tibble(a = 1, b = 1, c = 1),
                                "X%d.")),
               paste0("X", 1:3, "."))
})

test_that("rename_seq works with a function", {
  expect_equal(names(rename_seq(tibble(a = 1, b = 1, c = 1),
                                function(i) {
                                  paste0("variable_", i)
                                })),
               paste0("variable_", 1:3))
})

test_that("rename_map works as expected", {
  expect_equal(names(rename_map(tibble(a = 1, b = 2),
                                function(x) paste0("var_", x))),
               paste0("var_", c("a", "b")))
})

test_that("rename_map works as expected", {
  expect_equal(names(rename_map(tibble(a = 1, b = 2),
                                stringr::str_to_upper)),
               c("A", "B"))
})

test_that("rename_map works with an arbitrary function", {
  expect_equal(names(rename_map(tibble(a = 1, b = 2),
                                ~ stringr::str_to_upper(.x))),
               c("A", "B"))
})

test_that("rename_replace works as expected", {
  expect_equal(names(rename_replace(tibble(alpha = 1, beta = 2),
                                    "beta", "bravo")),
               c("alpha", "bravo"))
})

test_that("rename_replace works as expected", {
  expect_equal(names(rename_replace_all(tibble(alpha = 1, beta = 2),
                                    "a", "A")),
               c("AlphA", "betA"))
})