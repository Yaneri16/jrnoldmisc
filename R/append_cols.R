append_cols <- function(.data, expr, .names = NULL) {
  out <- eval(expr, envir = .data, enclos = parent.frame())
  if (!is.data.frame(out)) {
    stop("Results must a data frame", call. = FALSE)
  }
  if (nrow(out) == 1) {
    out <- map_df(seq_len(nrow(.data)), ~ out)
  }
  if (nrow(out) != nrow(.data)) {
    stop("Results must have either 1 or nrow(.data) rows", call. = FALSE)
  }
  if (!is.null(.names)) {
    names(out) <- .names
  }
  append_df(.data, out)
}