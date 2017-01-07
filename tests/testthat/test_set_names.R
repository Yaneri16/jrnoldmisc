context("set_names")

x <- c("a", "b", "c")
X <- matrix(1:6, nrow = 2)

test_that("set_names_seq works with defaults", {
  expect_named(set_names_seq(x), paste0("Var", seq_along(x)))
})

test_that("set_names_seq works with character", {
  expect_named(set_names_seq(x, "X_%d_X"), paste0("X_", seq_along(x), "_X"))
})

test_that("set_names_seq works with function", {
  expect_named(set_names_seq(x, function(i, sep) {
    paste0("Z", i, sep)
  },
  sep = "_"),
  paste0("Z", seq_along(x), sep = "_"))
})

test_that("set_names_map works as expected", {
  expect_equal(names(set_names_map(list(a = 1, b = 2),
                                function(x) paste0("var_", x))),
               paste0("var_", c("a", "b")))
})

test_that("set_names_map works as expected", {
  expect_equal(names(set_names_map(list(a = 1, b = 2),
                                stringr::str_to_upper)),
               c("A", "B"))
})

test_that("set_names_map works with an arbitrary function", {
  expect_equal(names(set_names_map(list(a = 1, b = 2),
                                ~ stringr::str_to_upper(.x))),
               c("A", "B"))
})

test_that("names_replace works as expected", {
  expect_equal(names(names_replace(list(alpha = 1, beta = 2),
                                    "beta", "bravo")),
               c("alpha", "bravo"))
})

test_that("set_names_replace works as expected", {
  expect_equal(names(names_replace_all(list(alpha = 1, beta = 2),
                                        "a", "A")),
               c("AlphA", "betA"))
})
