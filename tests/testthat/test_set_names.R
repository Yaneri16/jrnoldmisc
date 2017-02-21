context("set_names")

x <- c("a", "b", "c")
X <- matrix(1:6, nrow = 2)

test_that("set_names_idx works with defaults", {
  expect_named(set_names_idx(x), as.character(seq_along(x)))
})

test_that("set_names_idx works with character", {
  expect_named(set_names_idx(x, "X_%d_X"), paste0("X_", seq_along(x), "_X"))
})

test_that("set_names_idx works with function", {
  expect_named(set_names_idx(x, function(i, sep) {
    paste0("Z", i, sep)
  },
  sep = "_"),
  paste0("Z", seq_along(x), sep = "_"))
})

