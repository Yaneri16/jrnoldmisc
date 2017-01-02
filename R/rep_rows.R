#' Replicate rows of a data frame
#'
#' @param .data A data frame
#' @param times See \code{\link{rep}}
#' @param each See \code{\link{rep}}
#' @return A data frame with replicated rows
#' @export
#' @examples
#' require("tibble")
#' x <- tibble(a = 1:2, b = 3:4)
#' rep_rows(x, 2)
#' rep_rows(x, each = 2)
rep_rows <- function(.data, times = 1, each = NA) {
  idx <- rep(seq_len(nrow(.data)), each = each, times = times)
  .data[idx, ]
}
