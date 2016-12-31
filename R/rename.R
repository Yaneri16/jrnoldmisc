#' Rename tables with sequentially numbered variables
#'
#' @param .f Either a string or function. If a string, then it is a
#'   \code{sprintf} in which the 1st pattern will be replaced with the variable
#'   index number. If a function, then it is a function that takes the index
#'   nubmer as its first argument.
#' @param ... Additional arguments to pass to \code{.f}
#' @param .data A data frame
#' @export
rename_seq <- function(.data, .f = "Var%d", ...) {
  assert_that(is.string(.f) || is.function(.f))
  oldnames <- names(.data)
  newnames <- seq_names(seq_along(oldnames), .f, ...)
  rename_(.data, .dots = set_names(oldnames, newnames))
}


#' Rename tables with a function
#'
#' @param .data A data frame
#' @param .f A function that takes the names of \code{.data} as its first argument.
#' @param ... Additional arguments to pass to \code{.f}
#' @export
rename_map <- function(.data, .f, ...) {
  oldnames <- names(.data)
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
#' @param pattern,replacement Pattern and replacement regular expressions.
#'   See \code{\link[stringr]{str_replace}}.
#' @export
rename_replace  <- function(.data, pattern, replacement) {
  rename_map(.data, str_replace, pattern, replacement)
}

#' @rdname rename_replace
#' @export
rename_replace_all <- function(.data, pattern, replacement) {
  rename_map(.data, str_replace_all, pattern, replacement)
}
