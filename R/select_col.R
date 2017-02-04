#' Select single column from a table
#'
#' Select a single column from a table and
#' return as a vector. This is pretty-much
#' just a wrapper for `[[`, but is more
#' descriptive and pipe friendly.
#'
#' If the \code{nm} argument is used, then this
#' can be seen as the inverse of \code{\link[tibble]{enframe}}.
#'
#' @param col In \code{select_col} an unquoted
#'   column name. In \code{select_col_}, the
#'   column name as a string.
#' @param nm If non-\code{NULL}, names to apply to the vector.
#'   In \code{select_col} an unquoted column name.
#'   In \code{select_col_}, the column name as a string.
#' @param .data A data frame
#' @export
#' @examples
#' select_col(iris, Species)
#' select_col_(iris, "Species")
select_col <- function(.data, col, nm = NULL) {
  if (!is.null(nm)) {
    nm <- col_name(substitute(nm))
  }
  col <- col_name(substitute(col))
  select_col_(.data, col, nm = nm)
}

#' @rdname select_col
#' @importFrom assertthat is.string assert_that
#' @export
select_col_ <- function(.data, col, nm = NULL) {
  assert_that(is.string(col))
  assert_that(is.null(nm) | is.string(nm))
  x <- .data[[col]]
  if (!is.null(nm)) {
    names(x) <- .data[[nm]]
  }
  x
}