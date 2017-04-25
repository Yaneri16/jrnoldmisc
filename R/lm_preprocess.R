#' Preprocess modeling data
#'
#' Preprocess data using the same method as \code{\link[stats]{lm}} and \code{\link[stats]{glm}},
#' taking a \code{formula} and \code{data} and returning the response (\code{y}),
#' design matrix (\code{X}), weights (\code{weights}), offsets (\code{offsets}), and
#' other metadata, after consistently handling missing values.
#'
#' This function is similar to calling
#' \code{lm(..., method = "model.frame")} or \code{glm(..., method = "model.frame")}.
#'
#'
#' @param formula	 an object of class \code{"\link{formula}"} (or one that can be coerced to that class):
#'   a symbolic description of the model to be fitted. The details of model specification are given under ‘Details’.
#' @param data an optional data frame, list or environment (or object coercible by \code{\link{as.data.frame}} to a data frame)
#'   containing the variables in the model. If not found in data, the variables are taken from environment(formula),
#'   typically the environment from which the function is  is called.
#' @param subset	 an optional vector specifying a subset of observations to be used.
#' @param weights an optional vector of weights to be used. Should be \code{NULL} or a numeric vector.
#' @param na.action a function which indicates what should happen when the data contain \code{NAs}. The default is set #'   by the \code{\link[stats]{na.action}} setting of options, and is \code{na.fail} if that is unset.
#'   The ‘factory-fresh’ default is \code{\link[stats]{na.omit}}. Another possible value is \code{NULL}, no action.
#'   Value \code{\link[stats]{na.exclude}} can be useful.
#' @param offset  A known component to be included in the linear predictor during fitting. This should be \code{NULL} or #'    a numeric vector of length equal to the number of cases.
#'    One or more \code{\link{offset}} terms can be included in the formula instead or as well,
#'    and if more than one are specified
#'    their sum is used. See \code{\link[stats]{model.offset}}.
#' @param contrasts.arg A list, whose entries are values (numeric matrices or character strings naming functions)
#'    to be used as replacement values for the \code{\link[stats]{contrasts}} replacement function and whose
#'     names are the names of columns of data containing factors. See \code{model.matrix} for more details.
#' @param ... Additional arguments.
#' @return A list with elements
#' \describe{
#' \item{\code{y}}{vector or matrix. The response}
#' \item{\code{X}}{matrix. The design matrix}
#' \item{\code{offset}}{offset. The offset}
#' \item{\code{terms}}{A \code{\link{terms}} object}
#' \item{\code{xlevels}}{A named character vector with any levels used in the design matrix}
#' }
#' @seealso \code{\link[stats]{model.frame}}, \code{\link[stats]{model.matrix}}, \code{\link[stats]{model.weights}},
#'   \code{\link[stats]{model.offset}}
#' @export
#' @example
#' lm_preprocess(mpg ~ disp + am, data = mtcars)
lm_preprocess <- function(formula,
                          data = NULL,
                          weights = NULL,
                          contrasts.arg = NULL,
                          na.action = options("na.action"),
                          offset = NULL,
                          subset = NULL,
                          ...) {
  # most of this code copied from stats::lm and stats::glm
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data", "subset", "weights", "na.action",
               "offset"), names(mf), 0L)
  mf <- mf[c(1L, m)]
  mf$drop.unused.levels <- TRUE
  mf[[1L]] <- quote(stats::model.frame)
  mf <- eval(mf, parent.frame())
  mt <- attr(mf, "terms")
  y <- stats::model.response(mf, "any")
  if (length(dim(y)) == 1L) {
    nm <- rownames(y)
    dim(y) <- NULL
    if (!is.null(nm))
      names(y) <- nm
  }
  out <- list(
    y = y,
    w =  as.vector(stats::model.weights(mf)),
    offset = as.vector(stats::model.offset(mf)),
    X = model.matrix(mt, mf, contrasts.arg),
    terms = mt,
    xlevels = stats::.getXlevels(mt, mf)
  )
  out$n <- nrow(out$X)
  out$k <- ncol(out$X)
  out
}
