#' Rename tables with sequentially numbered variables
#' @rdname set_names_seq
#' @param .data A data frame
#' @export
rename_seq <- function(.data, .f = "Var%d", ..., vars = list()) {
  oldnames <- names(.data)
  newnames <- seq_names(seq_along(oldnames), .f, ...)
  rename_(.data, .dots = set_names(oldnames, newnames))
}


#' @rdname set_names_map
#' @param .data A data frame
#' @export
rename_map <- function(.data, .f, ...) {
  oldnames <- names(.data)
  newnames <- as_function(.f)(oldnames, ...)
  rename_(.data, .dots = set_names(oldnames, newnames))
}


#' @rdname set_names_sub
#' @param .data A data frame
#' @param pattern,replacement Pattern and replacement regular expressions.
#'   See \code{\link[stringr]{str_replace}}.
#' @param all If \code{TRUE}, then use \code{str_replace_all} to replace
#'   names, else use \code{str_replace}.
#' @export
rename_sub <- function(.data, pattern, replacement, all = TRUE) {
  assert_that(is.flag(all))
  FUN <- if (all) str_replace else str_replace_all
  rename_map(.data, FUN, pattern, replacement)
}
