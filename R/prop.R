#' Proportion of observations by group
#'
#' \code{prop} calculates the proportion of observations by group.
#' This is analaguous to \code{\link[dplyr]{count}} which counts the number of
#' rows by group. Alternatively, \code{\link[dplyr]{count}} is to \code{\link[base]{table}}
#' as \code{prop} is to \code{\link[base]{prop.table}}.
#'
#' @param x A \code{tbl}.
#' @param wt If omitted, will the number of rows. If specified, will perform a "weighted" proportion by summing the (non-missing) values of variable \code{wt}.
#' @param vars,... Variables to group by
#' @param sort If \code{TRUE}, sort output in descending order of \code{n}.
#' @return A \code{tbl} with the grouping variables and column \code{.prop} with the
#'   proportion in each group.
#' @export
prop <- function(x, ..., wt = NULL, sort = FALSE) {
  vars <- lazyeval::lazy_dots(...)
  wt <- substitute(wt)
  prop_(x, vars, wt, sort = sort)
}

#' @rdname prop
#' @export
prop_ <- function(x, vars, wt = NULL, sort = FALSE) {
  x <- count_(x, vars, wt = wt)
  g <- groups(x)
  # only makes sense to divide by total wt. Otherwise there is no
  # way to roll up the proportions because wt of each group is
  # lost.
  x <- mutate_(ungroup(x), .prop = ~ n / sum(n))
  # restore grouping similar to count
  x <- group_by_(x, .dots = lazyeval::as.lazy_dots(g))
  x <- select_(x, ~ -n)
  if (sort) {
    x <- arrange_(x, desc(~ .prop))
  }
  x
}