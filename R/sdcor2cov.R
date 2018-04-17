#' Standard deviations and Correlation to Covariance Matrix
#'
#' @param s A vector of standard deviations
#' @param r A correlation matrix with \code{dim(length(s), length(x)}.
#' @return A \code{dim(length(s), length(s))} covariance matrix.
#' @export
sdcor2cov <- function(s, r = diag(length(s))) {
  s <- diag(s, nrow = length(s), ncol = length(s))
  s %*% r %*% s
}
