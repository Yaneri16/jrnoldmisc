seq_names <- function(.x, .f, ...) {
  assert_that(is.string(.f) | is.function(.f))
  if (is.character(.f)) {
    sprintf(.f, .x, ...)
  } else {
    as_function(.f)(.x, ...)
  }
}

#' Set names in a vector with sequential numbering
#'
#' Set the names in vector with a pattern based on sequential numbering,
#' e.g. \code{Var1}, \code{Var2}, ...
#'
#' @param x An object to be named
#' @param .f A character vector or function. If \code{.f} is a function,
#'  then its first argument will be the index numbers of the names.
#'  If \code{.f} is a character, then it should be a format string
#'  for \code{sprintf} pattern.
#' @param ... Arguments passed to \code{.f}.
#' @return The object \code{x} with its names (colnames, rownames) set.
#' @seealso \code{\link[stats]{setNames}}, \code{\link[purrr]{set_names}},
#'   \code{\link[magrittr]{set_colnames}}, \code{\link[magrittr]{set_colnames}}.
#' @export
set_names_seq <- function(x, .f = "Var%d", ...) {
  set_names(x, seq_names(seq_along(x), .f, ...))
}

#' Set names in a vector with a function
#'
#' Set names in a vector by applying a function to the original names.
#'
#' @param x Vector to name
#' @param .f Function to use to name the variables.
#'    This is converted to a function using \code{\link[purrr]{as_function}}.
#'    The first argument will be \code{names(x)}, pass additional functions with
#'    \code{...}. It should return a character vector of the same length as the
#'    names of \code{x}.
#' @param ... Arguments passed to the function \code{.f}.
#' @return The vector \code{x} with new names
#' @export
set_names_map <- function(x, .f, ...) {
  newnames <- as_function(.f)(names(x), ...)
  set_names(x, newnames)
}

#' Set names in a vector by regular expression
#'
#' @param x Vector to name
#' @param pattern,replacement Pattern and replacement strings. See \code{\link[stringr]{str_replace}}
#' @param all Use \code{\link[stringr]{str_replace_all}},
#'   else use \code{\link{str_replace}}.
#' @return Return table with renamed variables
#' @export
names_replace <- function(x, pattern, replacement, all = TRUE) {
  assert_that(is.flag(all))
  set_names_map(x, str_replace, pattern, replacement)
}

#' @rdname names_replace
#' @export
names_replace_all <- function(x, pattern, replacement, all = TRUE) {
  assert_that(is.flag(all))
  set_names_map(x, str_replace_all, pattern, replacement)
}