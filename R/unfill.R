#' Unfill a vector
#'
#' @param x A vector
#' @param tokeep If \code{"first"}, then keep the first value. If \code{"last"},
#'   then keep the last value in the run. If \code{"middle"}, keep the middle
#'   value.
#' @param value The value to use to unfill the vector.
#' @export
unfill <- function(x, tokeep = c("first", "last", "middle"), value = NA) {
  runs <- rle(x)
  if (tokeep == "first") {
    idxs <- cumsum(c(1L, runs[-length(runs)]))
  } else if (tokeep == "last") {
    idxs <- cumsum(runs)
  } else if (tokeep == "middle") {
    starts <- cumsum(c(1L, runs[-length(runs)]))
    ends <- cumsum(runs)
    idxs <- (starts + ends) %/% 2L
  }
  x[!(seq_along(x) %in% idxs)] <- value
  x
}