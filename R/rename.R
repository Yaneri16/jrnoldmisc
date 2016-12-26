#' @rdname set_names_seq
#' @param .data A data frame
#' @export
rename_seq <- function(.data, .f = "Var%d", ...) {
  set_names_seq(.data, .f, ...)
}


#' @rdname set_names_map
#' @param .data A data frame
#' @export
rename_map <- function(.data, .f, ...) {
  set_names_map(.data, .f, ...)
}


#' @rdname set_names_sub
#' @param .data A data frame
#' @export
rename_sub <- function(.data, pattern, replacement, all = TRUE) {
  set_names_sub(.data, pattern, replacement, all = all)
}
