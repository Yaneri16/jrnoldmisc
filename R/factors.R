# copied from forcats:::check_factor
check_factor <- function(f) {
  if (is.character(f)) {
    factor(f)
  }
  else if (is.factor(f)) {
    f
  }
  else {
    stop("`f` must be a factor (or character vector).", call. = FALSE)
  }
}

#' Is a one- or two-sided formula?
#'
#' Functions to test whether an object is a one- or two-sided formula.
#'
#' @param x An object to test
#' @return \code{TRUE} or \code{FALSE}
#' @seealso \code{\link[purrr]{is_formula}} in \pkg{purrr} and \code{\link[lazyeval]{is_formula}} in \pkg{lazyeval} both test objects for formula.
#' @export
#' @examples
#' is_formula2(y ~ x)
#' is_formula2(~ x)
#' is_formula1(y ~ x)
#' is_formula1(~ x)
is_formula2 <- function(x) {
  purrr::is_formula(x) && (length(x) == 3)
}


#' @rdname is_formula2
#' @export
is_formula1 <- function(x) {
  purrr::is_formula(x) && (length(x) == 2)
}

#' Transform levels of a factor with a regular expression
#'
#' Replace the factor levels with a regular expression.
#' \code{fct_replace} replaces the factor levels using a regular
#'
#' @param f A factor
#' @param pattern,replacement Pattern and replacement regular expressions.
#'   See \code{\link[stringr]{str_replace}}.
#' @param all If \code{TRUE}, replace all occurences of \code{pattern},
#'   otherwise replace only the first occurrence.
#' @return A factor vector with the values of \code{f} and transformed levels.
#' @export
fct_sub <- function(f, pattern, replacement, all = TRUE) {
  f <- check_factor(f)
  FUN <- if (all) str_replace_all else str_replace
  old_levels <- levels(f)
  new_levels <- FUN(old_levels, pattern, replacement)
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
fct_idx <- function(f, .f = "%d", ...) {
  f <- check_factor(f)
  lvls_revalue(f, make_seq_names(seq_along(levels(f)), .f, ...))
}

#' Remove levels from a factor
#'
#' Remove levels from a factor, meaning that observations with those levels
#' are set to `NA`.
#'
#' @seealso \code{\link[forcats]{fct_recode}} which can
#'   remove factor levels and \code{\link[forcats]{fct_explicit_na}} which
#'   is the inverse, converting `NA` to a factor level.
#' @param f A factor
#' @param lvls Character vector of levels to remove
#' @return A factor
#' @export
#' @importFrom forcats lvls_revalue
#' @examples
#' f <- factor(c("Low", "Medium", "High",
#'               "Refused to respond", "No response", "Not asked"))
#' fct_remove(f, c("Refused to respond", "No response", "Not asked"))
fct_remove <- function(f, lvls) {
  f <- check_factor(f)
  factor(f, levels = setdiff(levels(f), as.character(lvls)))
}
