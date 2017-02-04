#' Check that an object has names
#'
#' @param x An object
#' @return A logical value
#' @family assertions
#' @export
#' @examples
#' has_names(c(1, 2, 3))
#' has_names(c(a = 1, b = 2, c = 3))
has_names <- function(x) {
  !is.null(names(x)) & !all(names(x) == "") & !all(is.na(names(x)))
}

on_failure(has_names) <- function(call, env) {
  paste0(deparse(call$x), " is not named")
}

#' Check that an object has unique names
#'
#' @param x An object
#' @return A logical value
#' @export
#' @examples
#' has_unique_names(list(a = 1, b = 2))
#' has_unique_names(list(a = 1, a = 2))
has_unique_names <- function(x) {
  dup_names <- duplicated(names(x))
  any(dup_names)
}

on_failure(has_unique_names) <- function(call, env) {
  paste0(deparse(call$x), " has duplicate names.")
}

#' Check that all names in an object are not-missing
#'
#' @param An object
#' @return A logical value
#' @export
#' @examples
#' has_no_missing_names <- list(1, b = 1)
#' has_no_missing_names <- list(a = 1, b = 1)
has_no_missing_names <- function(x) {
  nm <- x
  !is.null(nm) & all(!is.na(nm)) & all(nm != "")
}

on_failure(has_no_missing_names) <- function(call, env) {
  paste0(deparse(call$x), " has missing names.")
}
