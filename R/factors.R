.fct_replace <- function(f, pattern, replacement, all) {
  FUN <- if (all) str_replace_all else str_replace
  old_levels <- levels(f)
  new_levels <- FUN(old_levels, pattern, replacement)
  lvls_revalue(f, new_levels)
}

#' Transform levels of a factor with a regular expression
#'
#' Replace the factor levels with a regular expression.
#' \code{fct_replace} replaces the factor levels using
#' \code{\link[stringr]{str_replace}}, while \code{fct_replace_all} uses
#' \code{\link[stringr]{str_replace_all}}
#'
#' @param f A factor
#' @param pattern,replacement Pattern and replacement regular expressions.
#'   See \code{\link[stringr]{str_replace}}.
#' @return A factor vector with the values of \code{f} and transformed levels.
#' @export
fct_replace <- function(f, pattern, replacement) {
  .fct_replace(f, pattern, replacement, FALSE)
}

#' @rdname fct_replace
#' @export
fct_replace_all <- function(f, pattern, replacement) {
  .fct_replace(f, pattern, replacement, TRUE)
}


#' Transform levels of a factor with a function
#'
#' Change the factor levels by a function of their current values.
#'
#' @param f A factor
#' @param .f A function that takes the current levels as its first argument.
#' @param ... Arguments passed to \code{.f}.
#' @return A factor vector with the values of \code{f} and transformed levels.
#' @export
fct_map <- function(f, .f, ...) {
  warning("fct_map is deprecated, use fct_relabel in forcats >= 0.1.1.9000")
  old_levels <- levels(f)
  FUN <- as_function(.f)
  new_levels <- FUN(old_levels, ...)
  lvls_revalue(f, new_levels)
}

#' Transform levels of a factor with a function of their index
#'
#' Change the factor levels by a function or pattern based on their current
#' order, .e.g. 1, 2, 3, ....
#'
#' @param f A factor
#' @param .f If a character vector, a \code{\link{sprintf}} pattern in which the
#'   only argument will be the order of the factor levels. If a function, then
#'   a function in which the first argument is the order of the factor levels.
#' @param ... Arguments passed to \code{.f} if it is a function.
#' @return A factor vector with the values of \code{f} and transformed levels.
#' @export
fct_seq <- function(f, .f = "%d", ...) {
  if (!(is.function(.f) || is.character(.f))) {
    stop("Expected a function or string, got ", class(.f)[[1L]], call. = FALSE)
  }
  if (is.character(.f) && length(.f) > 1) {
    stop("Expected a character vector of length 1, but got one with length ",
         length(.f), call. = FALSE)
  }
  idx <- seq_along(levels(f))
  if (is.character(.f)) {
    new_levels <- sprintf(.f, idx)
  } else {
    new_levels <- .f(idx, ...)
  }
  lvls_revalue(f, new_levels)
}
