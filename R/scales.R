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
