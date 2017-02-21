context("fct_replace")

test_that("fct_replace works as expected", {
  fruits <- as.factor(c("one apple", "two pears", "three bananas"))
  expect_equal(levels(fct_sub(fruits, "[aeiou]", "-", all = FALSE)),
               c("-ne apple", "thr-e bananas", "tw- pears"))
})

test_that("fct_replace_all works as expected", {
  fruits <- as.factor(c("one apple", "two pears", "three bananas"))
  expect_equal(levels(fct_sub(fruits, "[aeiou]", "-", all = TRUE)),
               c("-n- -ppl-", "thr-- b-n-n-s", "tw- p--rs"))
})

test_that("fct_replace can collapse levels", {
  x <- as.factor(c("alpha", "alpha+", "a", "bravo", "b", "beta",
                        "charlie"))
  expect_equal(levels(fct_sub(x, "^([a-z]).*", "\\1", all = TRUE)),
               c("a", "b", "c"))
})
