#' @importFrom ggplot2 scale_x_continuous scale_y_continuous
#' @importFrom stats contr.treatment model.matrix setNames
#' @importFrom dplyr select_vars_ select_ vars select_vars filter distinct
#' @importFrom stringr str_replace str_replace_all
#' @importFrom purrr flatten keep cross_n "%||%" set_names as_function
#' @importFrom purrr map rerun
#' @importFrom stats complete.cases
#' @import assertthat
NULL

#' Test for equality with NA, NaN
#'
#' Like \code{x == y}, but not returning \code{NA} in tests involving
#' \code{NaN} or \code{NA}.
#'
#' @param x,y Values to test
#' @return A logical vector with the test results.
#' @examples
#' x <- c(1, NA)
#' y <- c(1, NA)
#'
#' @export
#' @examples
#' x <- c(0, 1, NA, NaN, NA, NaN)
#' y <- c(0, 0, NA, NaN, 0, 0)
#' x %==% y
"%==%" <- function(x, y) {
  z <- (is.na(x) & is.na(y)) | (is.nan(x) & is.nan(y)) | x == y
  z[is.na(z)] <- FALSE
  z
}

#' Replace NA with FALSE
#'
#' @param x A logical vector
#' @return The vector \code{x} with any \code{NA} values replaced with \code{FALSE}
#'
#' @export
#' @examples
#' x <- c(1, 2, NA) < 5
#' x
#' na2f(x)
na2f <- function(x) {
  z <- as.logical(x)
  z[is.na(z)] <- NA
  z
}
