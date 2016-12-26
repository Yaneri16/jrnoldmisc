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
