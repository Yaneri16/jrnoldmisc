#' Rename tables with sequentially numbered variables
#'
#' @param .f Either a string or function. If a string, then it is a
#'   \code{sprintf} in which the 1st pattern will be replaced with the variable
#'   index number. If a function, then it is a function that takes the index
#'   number as its first argument.
#' @param ... Additional arguments to pass to \code{.f}
#' @param .data A data frame
#' @export
rename_idx <- function(.data, .f = "Var%d", ...) {
  oldnames <- tbl_vars(.data)
  newnames <- make_seq_names(seq_along(oldnames), .f, ...)
  rename_(.data, .dots = set_names(oldnames, newnames))
}


#' Rename tables with a function
#'
#' @param .data A data frame
#' @param .f A function that takes the names of \code{.data} as its first argument.
#' @param ... Additional arguments to pass to \code{.f}
#' @export
rename_map <- function(.data, .f, ...) {
  # use tbl_vars so more general
  oldnames <- tbl_vars(.data)
  newnames <- as_function(.f)(oldnames, ...)
  rename_(.data, .dots = set_names(oldnames, newnames))
}

#' Rename tables with a regular expression
#'
#' Use a regular expression to rename tables.
#' The function \code{rename_replace} applies \code{\link[stringr]{str_replace}} to the names of the table,
#' while \code{rename_replace_all} applies \code{\link[stringr]{str_replace_all}}.
#'
#' @param .data A data frame
#' @param all If \code{TRUE}, the replace all patterns, else replace only the
#'   the first pattern.
#' @param pattern,replacement Pattern and replacement regular expressions.
#'   See \code{\link[stringr]{str_replace}}.
#' @export
rename_sub  <- function(.data, pattern, replacement, all = TRUE) {
  .f <- if (all) str_replace_all else str_replace
  rename_map(.data, .f, pattern, replacement)
}
