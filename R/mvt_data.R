#' Simulate Data From the Normal or t-Distributions
#'
#' Simulate data from normal or Student \eqn{t}-distributions.
#'
#' This function is a wrapper around \code{\link[mvtnorm]{rmvt}}
#' and \code{\link[mvtnorm]{rmvnorm}} functions which returns
#' a data frame instead of a matrix.
#'
#' @param n Number of simulations to draw.
#' @param scale A vector of standard deviations of the scale matrix.
#' @param df The degrees of freedom. If \code{Inf}, then the data are
#'   simulated from a normal distribution.
#' @param loc A vector of locations.
#' @return A \code{\link[tibble]{tibble}} with \code{length(loc)}
#'   columns and \code{n} rows.
#' @export
#' @importFrom stringr str_c
#' @importFrom tibble as_tibble
rmvtnorm_df <- function(n, loc = 0,
                        scale = rep(1, length(loc)),
                        R = diag(length(loc)), df = Inf) {
  template <- "X{.idx}"
  sigma <- sdcor2cov(scale, R)
  X <- if (is.finite(df)) {
    mvtnorm::rmvt(n, sigma = sigma, type = "shfted", delta = loc)
  } else {
    mvtnorm::rmvnorm(n, mean = loc, sigma = sigma)
  }
  # TODO: empirically whiten
  X <- as_tibble(X)
  colnames(X) <- str_c("X", seq_len(ncol(X)))
  X
}
