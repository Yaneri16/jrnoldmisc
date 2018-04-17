library("purrr")
library("lazyeval")
library("stringr")
library("dplyr")

expand_call <- function(x, envir = parent.frame()) {
  if (is.atomic(x)) {
    if (is.character(x)) {
      .m(x, envir = envir)
    } else {
      x
    }
  } else if (is.name(x)) {
    x
  } else if (is.call(x)) {
    # expand function
    if (identical(x[[1]], quote(.m))) {
      eval(lazyeval::call_modify(x, list(envir = envir)))
    } else if (identical(x[[1]], quote(.p))) {
      eval(lazyeval::call_modify(x, list(envir = envir)))
    } else if (identical(x[[1]], quote(.x))) {
      eval(lazyeval::call_modify(x, list(envir = envir)))
    } else if (identical(x[[1]], quote(.x_))) {
      eval(lazyeval::call_modify(x, list(envir = envir)))
    } else {
      as.call(lapply(x, expand_call, envir = envir))
    }
  } else if (is.pairlist(x)) {
    as.call(lapply(x, expand_call, envir = envir))
  } else {
    # User supplied incorrect input
    stop("Don't know how to handle type ", typeof(x),
         call. = FALSE)
  }
}

#' Formula interpolation
#'
#' Formula interpolation to expand functions in a formula into lists of
#' variables. The interpolation provided here are convenience functions for
#' formulae used in modeling applications.
#'
#' @details
#'
#' The following special interpolation functions are provided:
#' \itemize{
#' \item{\code{.x(..., sep = "+")} expands variables using the semantics in
#'    \code{dplyr} selection functions.}
#' \item{\code{.x_(..., sep = "+")} is the standard evaluation version of
#'    \code{.x}}
#' \item{\code{.p(.f, sep = "+")} matches all objects in \code{envir} that
#'    satisfy the predicate function \code{.f}}
#' \item{\code{.m(pattern, sep = "+")} matches all names in \code{envir} that
#'    match the regular expression \code{pattern}.}
#' }
#'
#' @param f A formula
#' @param lhs logical. Expand the left-hand side of the formula
#' @param rhs logical. Expand the left-hand side of the formula
#' @param envir An environment in which to evaluate the functions. This
#'   is where the expansion functions look for variable names.
#' @return A \code{formula} object
#' @seealso See \code{\link[base]{bquote}} and \code{\link[lazyeval]{f_interp}}
#'   for other formula interpolation methods.
#' @export
#' @importFrom lazyeval f_lhs f_lhs<- f_rhs f_rhs<-
#' @examples
#' f_expand(~ .m("Sepal\\.*"), envir = iris)
#' f_expand(~ .x(everything()), envir = iris)
#' f_expand(~ .x(-Sepal.Width), envir = iris)
#' f_expand(~ .x(matches("Length"), -Sepal.Length), envir = iris)
#' f_expand(~ .p(is.numeric), envir = iris)
#' f_expand(~ .p(is.numeric, sep = "*"), envir = iris)
f_expand <- function(f, lhs = TRUE, rhs = TRUE, envir = parent.frame()) {
  if (rhs) {
    f_rhs(f) <- expand_call(f_rhs(f), envir = envir)
  }
  if (lhs) {
    f_lhs(f) <- expand_call(f_lhs(f), envir = envir)
  }
  f
}

fcomb <- function(x, sep = "+", paren = FALSE) {
  out <- reduce(map(x, as.name), function(x, y) call(sep, x, y))
  if (paren) {
    out <- call("(", out)
  }
  out
}

.x <- function(..., sep = "+", envir = parent.frame()) {
  args <- lazyeval::lazy_dots(...)
  .x_(args, envir = envir, sep = sep)
}

#' @importFrom dplyr select_vars_
#' @importFrom purrr reduce map
.x_ <- function(args, sep = "+", envir = parent.frame()) {
  #if (is.character(sep)) sep <- as.name(sep)
  vars <- select_vars_(ls(envir, all.names = TRUE), args)
  if (length(vars) == 0) {
    stop("No variables found", call. = FALSE)
  }
  fcomb(vars, sep = "+")
}

#' @importFrom stringr str_subset
#' @importFrom purrr reduce map
.m <- function(pattern, sep = "+", envir = parent.frame()) {
  vars <- str_subset(ls(envir, all.names = TRUE), pattern)
  fcomb(vars, sep = "+")
}

#' @importFrom purrr reduce compact map flatten_chr
.p <- function(.f, sep = "+", envir = parent.frame()) {
  vars <-
    flatten_chr(compact(map(names(envir),
                                function(nm) if (.f(envir[[nm]])) nm
                                else NULL)))
  fcomb(vars, sep = "+")
}

