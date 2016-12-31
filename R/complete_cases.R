#' Keep complete cases of a table
#'
#' Filter a table by keeping only rows with no missing values.
#' This is more tidyverse friendly version of \code{complete.cases}, and
#' saves writing potentially long \code{\link[dplyr]{filter}} expressions.
#'
#' @param .data A \code{tbl}. Currently only in-memory data frames are supported.
#' @param ... Comma separated list of unquoted expressions. This uses the same
#'   semantics as \code{\link[dplyr]{select}}. You can select a subset of
#'   variables to check for missing values.
#' @param .dots Used to work around non-standard evaluation.
#'   See \code{vignette("nse")} for details.
#' @return The table with rows containing \code{NA} values dropped.
#' @author Jan Schultz
#' @references \url{http://stackoverflow.com/a/37031161/227406}
#' @export
complete_cases <- function(.data, ...) {
  complete_cases_(.data, .dots = lazyeval::lazy_dots(...))
}

#' @rdname complete_cases
#' @export
complete_cases_ <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ...)
  if (length(dots) == 0) {
    f <- complete.cases(.data)
  } else {
    f <- complete.cases(select_(.data, .dots = dots))
  }
  filter(.data, f)
}
