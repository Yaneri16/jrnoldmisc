# Copied from dplyr:::names2
names2 <- function(x) {
  names(x) %||% rep("", length(x))
}

seq_names <- function(.x, .f, ...) {
  assert_that(is.string(.f) | is.function(.f))
  if (is.character(.f)) {
    sprintf(.f, .x, ...)
  } else {
    as_function(.f)(.x, ...)
  }
}

#' Recode names of a vector
#'
#' @param .f A named character vector or something that
#'   can be coerced into a function. If it is a function
#'   it must return a character vector the same length as
#'   \code{names(x)}.
#' @param x A vector
#' @return The vector \code{x} with new names.
#' @export
recode_names <- function(x, .f, ...) {
  old_names <- names(.f)
  if (is_character(.f) & is.null(names(.f))) {
    if (is.null(names(.f))) {
      stop("If a character vector, .f must be named.",
           call. = FALSE)
    }
    bad_names <- setdiff(.f, old_names)
    if (length(bad_names)) {
        stop("All names in .f must be in names(x): ",
             paste0(unname(bad_names), collapse = ", "),
             call. = FALSE)
    }
    new_names <- set_names(old_names, old_names)
    new_names[old_names] <- .f
  } else if (is_function(.f)) {
    new_names <- as_function(.f)(old_names)
  }
  set_names(x, new_names)
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
#' @examples
#' set_names_idx(1:5)
#' set_names_idx(1:5, "X%d")
#' set_names_idx(1:5, function(i) stringr::str_c("Var_", i))
set_names_idx <- function(x, .f = "Var%d", ...) {
  set_names(x, seq_names(seq_along(x), .f, ...))
}


#' Set names in a vector by regular expression
#'
#' @param x Vector to name
#' @param pattern,replacement Pattern and replacement strings. See \code{\link[stringr]{str_replace}}
#' @param all Use \code{\link[stringr]{str_replace_all}},
#'   else use \code{\link[stringr]{str_replace}}.
#' @return Return table with renamed variables
#' @export
#' @examples
#' replace_names(c(a = 1, b = 2), c("beta" = "b"))
#' fruits <- c("one apple" = 1, "two pears" = 2, "three bananas" = 3)
#' replace_names(fruits, "[aeiou]", "-")
#' replace_names(fruits, "[aeiou]", "-", all = TRUE)
replace_names <- function(x, pattern, replacement, all = TRUE) {
  assert_that(is.flag(all))
  .f <- if (all) str_replace_all else str_replace
  set_names(x, .f(names(x), pattern, replacement))
}
