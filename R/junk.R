#' @importFrom ggplot2 scale_x_continuous scale_y_continuous
#' @importFrom purrr as_function
#' @import assertthat
#' @importFrom stats contr.treatment model.matrix setNames
#' @importFrom dplyr select_vars_
NULL

#' Log2 scale transformation
#'
#' Constructs log 2 continuous scales for \code{ggplot2}, similar
#' to \code{\link[ggplot2]{scale_x_log10}}.
#'
#' @param ... Passed to \code{\link[ggplot2]{scale_x_continuous}}
#'
#' @export
scale_x_log2 <- function(...) {
  scale_x_continuous(..., trans = scales::log2_trans())
}

#' @rdname scale_x_log2
#' @export
scale_y_log2 <- function(...) {
  scale_y_continuous(..., trans = scales::log2_trans())
}


seq_names <- function(.x, .f, ...) {
  assert_that(is.string(.f) | is.function(.f))
  if (is.character(.f)) {
    sprintf(.f, .x)
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
  purrr::set_names(x, seq_names(seq_along(x), .f, ...))
}


#' Truncate or censor vectors
#'
#' \code{truncate} will truncate a vector with replacing elements greater or less than
#' given bounds with replacement values (missing by default).
#' \code{censor} is a special case that uses the upper and bound values as the
#' replacements.
#'
#' @param x A vector
#' @param left,right Boundary values for truncation
#' @param leq,geq If \code{TRUE} use \code{<=} (\code{>=}) for tests at the boundary,
#'   otherwise use \code{<} (\code{>}).
#' @param newleft,newright Value to replace truncated elements
#' @return Vector of the same size as \code{x}, with truncated elements replaced.
#' @export
truncate <- function(x,
                     left = -Inf,
                     right = Inf,
                     newleft = NA,
                     newright = NA,
                     leq = FALSE,
                     geq = FALSE) {
  lt <- if (leq) `<=` else `<`
  gt <- if (geq) `>=` else `>`
  x[!is.na(x) & !is.nan(x) & lt(x, left)] <- newleft
  x[!is.na(x) & !is.nan(x) & gt(x, right)] <- newright
  x
}

#' @export
#' @rdname truncate
censor <- function(x, left = -Inf, right = Inf) {
  truncate(x, left = left, right = right, geq = TRUE, leq = TRUE,
           newleft = left, newright = right)
}

#' Does numeric vector fall in a range
#'
#' This is a shortcut for \code{x > lower & x < upper} and variants with equalities.
#' This function is more flexible than \code{\link[dplyr]{between}}.
#'
#' @param x Numeric vector of values
#' @param left,right Bountary values
#' @param geq,leq Use \code{>=} (\code{<=}) else use \code{>} (\code{<}) for testing
#'   the boundaries.
#' @param na.rm If \code{TRUE} return \code{FALSE} for \code{NA} or \code{NaN}
#' @return A logical vector that is \code{TRUE} if the value falls in the interval.
#' @export
between2 <- function(x, left = -Inf, right = Inf,
                     leq = TRUE, geq = TRUE, na.rm = TRUE) {
  lt <- if (leq) `<=` else `<`
  gt <- if (geq) `>=` else `>`
  inrange <- gt(x, left) & lt(x, right)
  if (na.rm) {
    inrange[is.na(inrange)] <- FALSE
  }
  inrange
}

#' Test for equality with NA, NaN
#'
#' Like \code{x == y}, but not returning \code{NA} in tests involving
#' \code{NaN} or \code{NA}.
#'
#' @param x,y Values to test
#' @return A logical vector with the test results.
#' @examples
#' x <- c(1, NA)
#' y <- c(1, NA)
#'
#' @export
#' @examples
#' x <- c(0, 1, NA, NaN, NA, NaN)
#' y <- c(0, 0, NA, NaN, 0, 0)
#' x %==% y
"%==%" <- function(x, y) {
  z <- (is.na(x) & is.na(y)) | (is.nan(x) & is.nan(y)) | x == y
  z[is.na(z)] <- FALSE
  z
}

#' Replace NA with FALSE
#'
#' @param x A logical vector
#' @return The vector \code{x} with any \code{NA} values replace with \code{FALSE}
#'
#' @export
#' @examples
#' x <- c(1, 2, NA) < 5
#' x
#' na2f(x)
na2f <- function(x) {
  z <- as.logical(x)
  z[is.na(z)] <- NA
  z
}
