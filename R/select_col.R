#' Select single column from a table
#'
#' Select a single column from a table and
#' return as a vector. This is pretty-much
#' just a wrapper for `[[`, but is more
#' descriptive and pipe friendly.
#'
#' @param col In \code{select_col} an unquoted
#'   column name. In \code{select_col_}, the
#'   column name as a string.
#' @param .data A data frame
#' @export
#' @examples
#' select_col(iris, Species)
#' select_col_(iris, "Species")
select_col <- function(.data, col) {
  col <- col_name(substitute(col))
  select_col_(.data, col)
}

#' @rdname select_col
#' @export
select_col_ <- function(.data, col) {
  assert_that(is.string(col))
  .data[[col]]
}