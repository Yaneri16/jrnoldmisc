append_cols <- function(.data, expr, .names = NULL) {
  append_cols_(.data, lazyeval::lazy(expr), .names = .names)
}

append_cols_ <- function(.data, expr, .names = NULL) {
  out <- tibble::as_tibble(lazyeval::lazy_eval(expr, .data))
  if (nrow(out) == 1) {
    out <- map_df(seq_len(nrow(.data)), ~ out)
  }
  if (nrow(out) != nrow(.data)) {
    stop("Results must have either 1 or nrow(.data) rows", call. = FALSE)
  }
  if (!is.null(.names)) {
    out <- set_names(out)
  }
  append_df(.data, out)
}