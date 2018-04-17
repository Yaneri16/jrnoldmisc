# from broom:::unrowname
unrowname <- function(x) {
  rownames(x) <- NULL
  x
}

# from broom::fix_data_frame
fix_data_frame <- function (x, newnames = NULL, newcol = "term") {
  if (!is.null(newnames) && length(newnames) != ncol(x)) {
    stop("newnames must be NULL or have length equal to number of columns")
  }
  if (all(rownames(x) == seq_len(nrow(x)))) {
    ret <- data.frame(x, stringsAsFactors = FALSE)
    if (!is.null(newnames)) {
      colnames(ret) <- newnames
    }
  }
  else {
    ret <- data.frame(...new.col... = rownames(x), unrowname(x),
                      stringsAsFactors = FALSE)
    colnames(ret)[1] <- newcol
    if (!is.null(newnames)) {
      colnames(ret)[-1] <- newnames
    }
  }
  unrowname(ret)
}

# adapted from broom:::tidy.summary.lm
tidy.summary.ivreg <- function(x, ...) {
  co <- stats::coef(x)
  nn <- c("estimate", "std.error", "statistic", "p.value")
  ret <- fix_data_frame(co, nn[1:ncol(co)])
  ret
}

process_ivreg <- function(ret, x, conf.int = FALSE, conf.level = 0.95) {
  if (conf.int) {
    CI <- suppressMessages(stats::confint(x, level = conf.level))
    colnames(CI) = c("conf.low", "conf.high")
    ret <- cbind(ret, unrowname(CI))
  }
  ret$estimate <- ret$estimate
  ret
}

#' Tidiers for \code{ivreg} objects
#'
#' Tidiers for \code{ivreg} objects returned by the
#'
#' These methods tidy the coefficients of an instrumental
#' variable model produced by the \pkg{AER} function
#' \code{\link[AER]{ivreg}} into a summary, augment the
#' original data with information on the fitted values
#' and residuals, and construct a one-row glance of the
#' model's statistics.
#'
#' @param x \code{ivreg} object
#' @param conf.int whether to include a confidence interval
#' @param conf.level confidence level of the interval, used only if \code{conf.int=TRUE}
#' @param quick whether to compute a smaller and faster version, containing only the term and estimate columns.
#' @param ... Extra arguments
#' @param data Original data, defaults to the extracting it from the model
#' @param newdata If provided, performs predictions on the new data
#' @param df the degrees of freedom to be used. See \code{\link[AER]{summary.ivreg}}.
#' @param diagnostics If \code{TRUE}, carry out diagnostic tests for the instrumental-variable regression. See \code{\link[AER]{summary.ivreg}}.
#' @return All tidying methods return a data.frame without rownames. The structure depends on the method chosen.
#'
#' @export
tidy.ivreg <- function(x, conf.int = FALSE, conf.level = 0.95,
                       quick = FALSE, ...) {
  if (quick) {
    co <- stats::coef(x)
    ret <- data.frame(term = names(co), estimate = unname(co))
    return(process_ivreg(ret, x, conf.int = FALSE))
  }
  s <- summary(x)
  ret <- tidy.summary.ivreg(s)
  process_ivreg(ret, x, conf.int = conf.int, conf.level = conf.level)
}

#' @export
#' @rdname tidy.ivreg
augment.ivreg <- function(x, data = stats::model.frame(x), newdata = NULL, ...) {
  res <- broom::augment_columns(x, data = data, newdata = NULL, ...)
  res$.hat <- stats::hatvalues(x)
  res
}

#' @export
#' @rdname tidy.ivreg
glance.ivreg <- function(x, df = NULL, diagnostics = FALSE, ...) {
  s <- summary(x, diagnostics = diagnostics, ...)
  ret <-
    with(s,
         data.frame(r.squared = r.squared,
                    adj.r.squared = adj.r.squared,
                    sigma = sigma,
                    waldtest.statistic = waldtest[1],
                    waldtest.p.value = waldtest[2],
                    waldtest.df2 = waldtest[3]))
  ret$df <- x$df.residual
  diag <- s$diagnostics
  if (!is.null(diag)) {
    ret$weak.instruments.statistic = diag["Weak instruments", "statistic"]
    ret$weak.instruments.p.value = diag["Weak instruments", "p-value"]
    ret$wu.hausman.statistic = diag["Wu-Hausman", "statistic"]
    ret$wu.hausman.p.value = diag["Wu-Hausman", "p-value"]
    ret$wu.hausman.df1 = diag["Wu-Hausman", "df2"]
    ret$wu.hausman.df2 = diag["Wu-Hausman", "df2"]
    ret$wu.hausman.df1 = diag["Wu-Hausman", "df1"]
    ret$wu.hausman.df2 = diag["Wu-Hausman", "df2"]
    ret$sargan.statistic = diag["Sargan", "statistic"]
    ret$sargan.df = diag["Sargan", "df"]
    ret$sargan.p.value = diag["Sargan", "p-value"]
  }
  rownames(ret) <- NULL
  ret
}

