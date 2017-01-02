#' Replicate rows of a data frame
#'
#' @param .data A data frame
#' @param times See \code{\link{rep}}
#' @param each See \code{\link{rep}}
#' @return A data frame with replicated rows
rep_rows <- function(.data, times = NA, each = NA) {
  idx <- rep(rep_rows, each = each, times = times)
  .data[idx, ]
}