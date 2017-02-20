#' Leave-on-out cross-validation
#'
#' Leave one out cross-validation. This is a convenience function
#' for \code{\link[modelr]{crossv_kfold}} when \code{k = nrow(data)}.
#'
#' @param data A data frame
#' @param id Name of variable that gives each model a unique integer id.
#' @return A data frame with n rows and columns \code{test} and \code{train}.
#'   \code{test} and \code{train} are list-columns containing
#'   \code{\link{resample}} objects.
#' @export
crossv_loo <- function(data, id = ".id") {
  crossv_kfold(data, nrow(data), id = id)
}

#' Generate jackknife replicates
#'
#' Generate \code{n} jacknife replicates, each of which is \code{n - 1}
#' rows.
#'
#' @param data A data frame
#' @param id Name of variable that gives each model a unique integer id.
#' @return A data frame containing a single column, \code{jackknife},
#'   which is a list-columns containing \code{\link{resample}} objects.
#' @export
jacknife <- function(data, id = ".id") {
  idx <- seq_len(nrow(data))
  jack <- map(idx, function(i) resample_jacknife(data, i))
  df <- tibble::tibble(jacknife = jack)
  df[[id]] <- idx
  df
}

#' Generate a jacknkife replicate
#'
#' Generate a jacknife replicate, which includes all rows of \code{data},
#' but a given row \code{i}.
#'
#' @param data A data frame
#' @param i The row index number to exclude
#' @return A \code{\link[modelr]{resample}} object.
#' @export
resample_jackknife <- function(data, i) {
  idx <- seq_len(nrow(data))
  resample(data, setdiff(idx, i))
}

sum2one <- function(x) x / sum(x)

#' Generate a permutation replicate
#'
#' Generate a permutation replicate, which randomly shuffles the row of
#' the data frame.
#'
#' @param data A data frame
#' @return A \code{\link[modelr]{resample}} object.
#' @export
resample_permutation <- function(data) {
  resample(data = data, idx = sample.int(nrow(data), replace = FALSE))
}

#' Generate permutation replicates
#'
#' Generate permutation replicates, which randomly shuffles the row of
#' the data frame.
#'
#' @param n Number of replicates to draw
#' @param data A data frame
#' @return A data frame with a single column named \code{perm}.
#'   This column is a list column of \code{\link[dplyr]{resample}} objects.
#' @export
permute <- function(data, n, id = ".id") {
  perm <- purrr::rerun(n, resample_permutation(data))
  df <- tibble::tibble(perm = perm)
  df[[id]] <- id(n)
  df
}


#' Generate a bootstrap replicate
#'
#' Generate a bootstrap replicate
#'
#' @param data A data frame
#' @param size Size of the replicate. It does not need to be the same as the
#'   the number of rows in \code{data}.
#' @param prob Observation specific probability weights.
#' @param replace logical If \code{TRUE}, resample with replacement
#'
resample_bootstrap2 <- function(data,
                                size = NULL,
                                wt = NULL,
                                alpha = NULL,
                                replace = FALSE) {
  n <- nrow(data)
  size <- size %||% n
  # Bayesian bootstrap
  if (!is.null(alpha)) {
    assert_that(is.numeric(alpha))
    assert_that(length(alpha) %in% c(1L, nrow(data)))
    if (!is.null(prob)) {
      # incorporate wts into alpha
      alpha <- sum2one(wt) * n * alpha
    }
    # sample probabilities from Dirichlet
    prob <- sum2one(rexp(nrow(data), alpha))
  } else {
    prob <- wt
  }
  idx <- sample.int(n, size, prob = wt, replace = replace)
  resample(data, idx)
}


#' first n elements
first_n <- function(x, n) {
  if (length(x) > n) x[1:n]
  else x
}

group_idx <- function(.data) {
  idx <- attr(.data, "indices")
  if (is.null(idx)) {
    idx <- list(seq_len(nrow(.data)))
  }
  idx
}

resample_groups <- function(data, .f, ...) {
  # indices for each group
  idx <- group_idx(data)
  gids <- seq_along(idx)
  grps <- as.integer(.f(tibble(groups = gids), ...))
  data_frame(.groupid = grps, resample = map(grps, resample, data = data))
}


# adapted from boot:::ts.array
ts_idx <- function(n, n.sim, l, sim, endcorr) {
  endpt <- if (endcorr)
    n
  else n - l + 1
  if (sim == "geom") {
    len.tot <- 0
    lens <- NULL
    while (len.tot < n.sim) {
      temp <- 1 + rgeom(1, 1/l)
      temp <- pmin(temp, n.sim - len.tot)
      lens <- c(lens, temp)
      len.tot <- len.tot + temp
    }
    st <- sampl.int(endpt, length(lens), replace = TRUE)
  }
  else {
    nn <- ceiling(n.sim / l)
    lens <- c(rep(l, nn - 1), 1 + (n.sim - 1) %% l)
    st <- sample.int(endpt, nn, replace = TRUE)
  }
  map2_int(st, lens, function(s, l) {
    if (l > 1) seq(s, s + l - 1L)
    else integer()
  })
}

#' Generate a time-series bootstrap replicate
#'
#' @param data A data frame
#' @param len If \code{sim} is \code{"fixed"} then \code{len} is the fixed block
#'    length used in generating the replicate time series.
#'    If \code{sim} is \code{"geom"} then \code{len} is the mean of the
#'    geometric distribution used to generate the block lengths.
#'    \code{length} should be a positive integer less than the number of rows
#'    of \code{data}.
#' @param sim The type of simulation required to generate the replicate time series.
#'    The possible input values are "model" (model based resampling), "fixed" (block resampling with fixed block lengths of l), "geom" (block resampling with block lengths having a geometric distribution with mean l)
#' @param size Length of the simulated time series in the replicate.
#' @param A logical variable indicating whether end corrections are to be applied when sim is "fixed". When sim is "geom", endcorr is automatically set to TRUE; endcorr is not used when sim is "model" or "scramble".
#' @seealso \code{\link[boot]{tsboot}}
#' @export
resample_ts_bootstrap <- function(data,
                                  size = NULL,
                                  len = NULL,
                                  sim = c("fixed", "geom"),
                                  endcorr = FALSE) {
  assert_that(is.flag(endcorr))
  n <- nrow(data)
  size <- size %||% n
  assert_that(is.number(size))
  assert_that(size > 0)
  assert_that(is.number(len))
  assert_that(len > 0 & len < n)
  sim <- match.arg(sim)
  idx <- ts_idx(n, size, length, sim, endcorr)
  resample(data, idx)
}


ts_bootstrap <- function(data,
                         size = NULL,
                         len = NULL,
                         sim = c("fixed", "geom"),
                         endcorr = FALSE) {
  perm <- purrr::rerun(n, resample_permutation(data))
  df <- tibble::tibble(perm = perm)
  df[[id]] <- id(n)
  df
}

#' Generate Poisson Bootstrap Replicates
#'
#' Generates replicates from a Poisson bootstrap, which
#' samples each observation independently from a Poisson distribtuion
#' with \expr{lambda = 1} if all obs have equal probability.
#'
#' @param data a data frame
#' @param n Number of replicates to draw
#' @param size Expected size of each replicate sample
#' @param wt Observation weights. If \code{NULL}, all observations
#'   are weighted equally.
#' @export
pois_bootstrap <- function(data, n, size = NULL, wt = NULL) {
  strap <- purrr::rerun(n, resample_pois_bootstrap(data))
  df <- tibble::tibble(strap = strap)
  df[[id]] <- seq_len(nrow(df))
  df
}

#' @rdname pois_bootstrap
#' @export
resample_pois_bootstrap <- function(data, size = NULL, wt = NULL) {
  n <- nrow(data)
  size <- size %||% n
  if (is.null(wt)) {
    lambda <- 1
  } else {
    lambda <- sum2one(wt) * n
  }
  # adjust to get expected size
  if (size != n) {
    lambda <- lambda * (size / n)
  }
  idx <- flatten_dbl(map2(seq_len(n), rpois(n, lambda),
                          funcion(i, m) rep(i, m)))
  resample(data, idx)
}


#' Generate n balanced bootstrap replicates
#'
#' @param data A data frame
#' @param n number of replicates
#' @param strata strata
#' @param clusters clusters
balanced_bootstrap <- function(data, n,
                               strata = FALSE,
                               clusters = FALSE) {
  if (strata) {
    idx <- group_idx(data)
    f <- function(x, B) {
      as.data.frame(matrix(sample(rep(x, times = B)), ncol = n))
    }
    map(unname(as.list(map_df(idx, f, B = B))),
        resample, data = data)
  } else {
    map(as.list(as.data.frame(matrix(sample(rep(idx, times = n)), ncol = B))),
        resample, data = data)
  }
}
