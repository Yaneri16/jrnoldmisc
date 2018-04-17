#' Create an equicorrelated matrix
#'
#' Create an correlation matrix in which all off-diagonal
#' elements are equal,
#' \deqn{
#' \begin{bmatrix}
#' 1 & \rho & \dots & rho \\
#' \rho & 1 & \dots & rho \\
#' \vdots & \vdots & \ddots & \vdots \\
#' \rho & \rho & \dots & 1
#' \end{bmatrix}
#' }
#'
#' @param k An integer with the matrix dimension.
#' @param rho The correlation coefficient for off-diagonal entries.
#' @param scale If \code{NULL} a correlation matrix is returned.
#'   If a vector of standard deviations, then a covariance matrix is returned.
#' @return A correlation or covariance matrix (if \code{scale} is not-null).
#' @export
equicorr <- function(k, rho = 0, scale = NULL) {
  R <- matrix(rho, nrow = k, ncol = k)
  diag(R) <- 1
  if (!is.null(scale)) {
    R <- t(scale) %*% R %*% scale
  }
  R
}