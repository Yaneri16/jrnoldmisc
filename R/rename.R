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


#' Select helper to select variables based on name glob pattern.
#'
#' Like \code{\link[dplyr]{matches}}, this function selects variables by by name.
#' However, it uses wildcard, aka globbing patterns instead of regular expressions.
#'
#' @details
#'
#' Use \code{?} to match a single character, and \code{*} to match any number of
#' characters, including none. The pattern is anchored at the start and end of the
#' string, meaning it must start at the start, and end at the the end.
#'
#' This function uses \code{\link[utils]{glob2rx}} to convert the globbing pattern
#' to a regex,
#'
#' @param pattern Globbing pattern
#' @param ignore.case If \code{TRUE}, the default, ignores case when matching names.
#' @param vars A character vector of variable names.
#'    When called from inside \code{select()} these are automatically set
#'    to the names of the table.
#' @return An integer vector given the position of the matched variables.
#' @export
glob <- function(pattern, ignore.case = TRUE, vars = current_vars()) {
  # no reason to drop or not  trim.head, trim.tail. it only simplifies
  # the regex, doesn't change behavior.
  matches(glob2rx(pattern), ignore.case = ignore.case, vars = vars)
}
