#' Convert column to dummy variables
#'
#' \code{to_dummies} converts a column into multiple columns of dummy variables.
#' \code{to_dummies_} is the standard evaluation version of this function.
#'
#' @param .data A \code{tbl}.
#' @param col In \code{to_dummies}, the bare (unquoted) column name.
#'   In \code{to_dummies_}, a character vector with column name.
#' @param sep The new variable names are constructed from \code{col} and the
#'   level value separated by \code{sep}.
#' @param exclude Ignore these levels of \code{col} when creating variables.
#' @return A \code{tbl}.
#' @export
to_dummies <- function(.data, col, sep = "_", exclude = NULL) {
  to_dummies_(.data, substitute(col), sep = sep)
}

#' @rdname to_dummies
#' @export
to_dummies_ <- function(.data, col, sep = "_", exclude = NULL) {
  # I use
  vals <- setdiff(as.character(distinct_(select_(.data, col))[[1]]),
                  exclude)
  dots <- set_names(map(vals, function(i, col) {
    lazyeval::interp(~ as.integer(col == i), i = i, col = as.name(col))
  }, col = col), paste(col, vals, sep = sep))
  .data <- mutate_(.data, .dots = dots)
  .data
}


#' Convert dummy variable columns to a categorical variable column
#'
#' \code{from_dummies} converts dummy variable columns to a categorical variable
#' column.
#' \code{from_dummies_} is the standard evaluation version.
#'
#' @param .data A \code{tbl}
#' @param col In \code{from_dummies}, the bare (unquoted) column name.
#'   In \code{from_dummies_}, a character vector with column name.
#' @param vars A list of columns generated by \code{\link[dplyr]{vars}}(),
#'    or a character vector of column names, or a numeric vector of column.
#'    These variables will be coerced to \code{logical}.
#' @param .default The value to use for rows in which all variables
#'   are \code{FALSE}.
#' @return A \code{tbl} with the additional column \code{col}.
#' @export
from_dummies <- function(.data, col, vars, .default = NA_character_) {
  from_dummies_(.data, substitute(col), vars, .default = .default)
}

#' @rdname from_dummies
#' @export
from_dummies_ <- function(.data, col, vars, .default = NA_character_) {
  assert_that(length(.default) == 1L)
  if (length(vars) == 0) {
    vars <- lazyeval::lazy_dots(everything())
  }
  vars <- select_vars_(tbl_vars(tbl), vars)
  .default <- as.character(.default)
  n <- nrow(.data)
  replaced <- rep(FALSE, n)
  out <- rep(.default, n)
  for (i in vars) {
    query <- !is.na(.data[[i]]) & as.logical(.data[[i]])
    out[query & !replaced] <- i
    replaced <- replaced | query
    if (all(replaced)) {
      break
    }
  }
  out
}
