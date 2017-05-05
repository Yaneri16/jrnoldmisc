#' Standardize and Unstandardize Coefficients
#'
#' Given a vector of \eqn{K} coeficients (\eqn{\beta}{beta}), and the means (\eqn{\bar{x}}{mean(x)}) and standard deviations (\eqn{s_x}(sd(x))) of X, \code{standardize} standardizes them.
#' The intercept is,
#' \deqn{
#' \beta_0^* = \frac{\beta_0 - \sum_{k = 1}^K \beta_k \frac{\bar{x_k}}{s_x} - \bar{y}}{s_y}
#' }{
#' beta_0^* = (beta_0 - sum(beta * x / sd(x) - mean(y))) / sd(y)
#' }
#' and the other coefficients are,
#' \deqn{
#' beta_k^* = \frac{\beta_k}{s_x s_y},
#' }{beta^* = beta / (sd(x) * sd(y)),}
#' for \eqn{k = 1, \dots, K}{k = 1, ..., K}.
#'
#' The function \code{unstandardize} converts standardized coefficients back
#' to their unstandardized forms.
#' For the intercept, the conversion is
#' \deqn{
#' \beta_0 = s_y \left(\beta_0 + \sum_{k} \beta_k^*  \times  mean(x_k) \times sd(x_k) \right) + \bar{y} .
#' }{
#' beta^* = sd_y * (beta + sum(beta * mean(x) * sd(x))) + mean(y)
#' }
#' For the other coefficients, the conversion is,
#' \deqn{
#' \beta_k = \beta_k^* s_x s_y
#' }{beta^* = beta * sd(x) * sd(y)}
#'
#' @param beta numeric vector with length \code{K} of the the coefficients.
#' @param x_mean numeric vector of length \code{K - 1}, with the means of the associated variables.
#' @param x_sd numeric vector of length \code{K - 1}, with the standard deviations of the associated variables.
#' @param y_mean number with the mean of the response variable.
#' @param y_sd number with the standard deviation of the response variable.
#' @return A numeric vector of length \code{K}.
#' @importFrom assertthat assert_that is.number
#' @export
standardize_coef = function(beta, x_mean = 0, x_sd = 1, y_mean = NULL, y_sd = NULL) {
  assert_that(length(x_mean) %in% c(1L, length(beta) - 1L))
  assert_that(length(x_sd) %in% c(1L, length(beta) - 1L))
  if (!is.null(y_mean)) assert_that(is.number(y_mean))
  if (!is.null(y_sd)) assert_that(is.number(y_sd))
  a = beta[1L]
  b = beta[-1L]
  bb = b / x_sd
  aa = a - sum(bb * x_mean)
  out <- c(aa, bb)
  if (!is.null(y_sd)) {
    out <- out / y_sd
  }
  if (!is.null(y_mean)) {
    out[1] <- out[1] + y_mean
  }
  out
}

#' @export
#' @rdname standardize_coef
unstandardize_coef = function(beta, x_mean = 0, x_sd = 1, y_mean = NULL, y_sd = NULL) {
  assert_that(length(x_mean) %in% c(1L, length(beta) - 1L))
  assert_that(length(x_sd) %in% c(1L, length(beta) - 1L))
  if (!is.null(y_mean)) assert_that(is.number(y_mean))
  if (!is.null(y_sd)) assert_that(is.number(y_sd))
  out <- if (!is.null(y_sd)) {
    beta * y_sd
  } else beta
  a <- out[1L]
  b <- out[-1L]
  bb <- b * x_sd
  aa <- a + b * x_mean + if (!is.null(y_mean)) y_mean else 0
  out <- c(aa, bb)
}
