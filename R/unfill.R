#' Unfill a vector
#'
#' Replace repeated runs of a value in a vector with a single
#' value.
#'
#' @param x A vector
#' @param tokeep If \code{"first"}, then keep the first value. If \code{"last"},
#'   then keep the last value in the run. If \code{"middle"}, keep the middle
#'   value.
#' @param value The value to use to unfill the vector.
#' @return The vector \code{x} with the elements other than those
#'   specified in \code{tokeep}, replaced with \code{value}.
#' @export
#' @examples
#' unfill(c(rep("a", 3), rep("b", 2)))
#' unfill(c(rep("a", 3), rep("b", 2)), value = "...")
#' unfill(c(rep("a", 3), rep("b", 2)), "first")
#' unfill(c(rep("a", 3), rep("b", 2)), "middle")
unfill <- function(x, tokeep = c("first", "last", "middle"), value = NA) {
  runs <- rle(x)
  tokeep <- match.arg(tokeep)
  if (tokeep == "first") {
    idxs <- cumsum(c(1L, runs[["lengths"]][-length(runs[["lengths"]])]))
  } else if (tokeep == "last") {
    idxs <- cumsum(runs[["lengths"]])
  } else if (tokeep == "middle") {
    starts <- cumsum(c(1L, runs[["lengths"]][-length(runs[["lengths"]])]))
    ends <- cumsum(runs[["lengths"]])
    idxs <- (starts + ends) %/% 2L
  }
  x[!(seq_along(x) %in% idxs)] <- value
  x
}
