#' Filter rows with missing values
#'
#' Remove rows with missing values in variables that are selected.
#'
#' @param .data A \code{tbl}
#' @param ... Comma separated list of unquoted expressions. Select variables
#'   which to test for missing values using the same semantics as
#'   \code{\link[dplyr]{select}}.
#' @param .all If \code{TRUE}, then remove rows in which all columns are
#'   missing, else select rows in which any column has a missing value.
#' @param .dots use \code{filter_na_()} to do standard evaluation. See
#'   \code{vignette("nse")} for details.
#' @param .finite For numeric columns, filter using \code{\link{is.finite}},
#' which will drop rows with \code{NaN}, \code{-Inf}, and \code{Inf} values in
#' addition to \code{NA}.
#' @return An object of the same class as \code{.data}.
#' @export
filter_na <- function(.data, ..., .all = FALSE, .finite = FALSE) {
  filter_na_(.data, .dots = lazyeval::lazy_dots(...),
             .all = .all, .finite = .finite)
}

#' @rdname filter_na
#' @export
filter_na_ <- function(.data, ..., .dots, .all = FALSE, .finite = FALSE) {
  assert_that(is.flag(.all))
  assert_that(is.flag(.finite))
  if (.finite & inherits(.data, "tbl_lazy")) {
    stop("The .finite option currently requires local sources",
         call. = FALSE)
  }
  dots <- lazyeval::all_dots(.dots, ...)
  if (length(dots) == 0) {
    dots <- list(~ everything())
  }
  vars <- select_vars_(names(.data), dots)
  # construct expression for filter
  # !is.na(var1), !is.na(var2), ...
  if (.finite) {
    numeric_vars <- map_lgl(.data, is_numeric)
    args <- map2(vars, numeric_vars,
                function(i, p) {
                  if (p) {
                    call("is.finite", as.name(i))
                  } else {
                    call("!", call("is.na", as.name(i)))
                  }
                })
  } else {
    args <- map(vars, function(i) call("!", call("is.na", as.name(i))))
  }
  if (.all) {
    #  !is.na(var1) | !is.na(var2) | ...
    args <- reduce(args, function(x, y) call("|", x, y))
  }
  args <- unname(args)
  filter_(.data, .dots = args)
}
