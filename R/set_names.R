# Copied from dplyr:::names2
names2 <- function(x) {
  names(x) %||% rep("", length(x))
}

seq_names <- function(.x, .f, ...) {
  assert_that(is.string(.f) | is.function(.f))
  if (is.character(.f)) {
    sprintf(.f, .x, ...)
  } else {
    as_function(.f)(.x, ...)
  }
}

#' Recode names of a vector
#'
#' @param .f A named character vector or something that
#'   can be coerced into a function. If it is a function
#'   it must return a character vector the same length as
#'   \code{names(x)}.
#' @param x A vector
#' @return The vector \code{x} with new names.
#' @export
recode_names <- function(x, .f, ...) {
  old_names <- names(.f)
  if (is_character(.f) & is.null(names(.f))) {
    if (is.null(names(.f))) {
      stop("If a character vector, .f must be named.",
           call. = FALSE)
    }
    bad_names <- setdiff(.f, old_names)
    if (length(bad_names)) {
        stop("All names in .f must be in names(x): ",
             paste0(unname(bad_names), collapse = ", "),
             call. = FALSE)
    }
    new_names <- set_names(old_names, old_names)
    new_names[old_names] <- .f
  } else if (is_function(.f)) {
    new_names <- as_function(.f)(old_names)
  }
  set_names(x, new_names)
}

#' Set names in a vector with sequential numbering
#'
#' Set the names in vector with a pattern based on sequential numbering,
#' e.g. \code{Var1}, \code{Var2}, ...
#'
#' @param x An object to be named
#' @param .f A character vector or function. If \code{.f} is a function,
#'  then its first argument will be the index numbers of the names.
#'  If \code{.f} is a character, then it should be a format string
#'  for \code{sprintf} pattern.
#' @param ... Arguments passed to \code{.f}.
#' @return The object \code{x} with its names (colnames, rownames) set.
#' @seealso \code{\link[stats]{setNames}}, \code{\link[purrr]{set_names}},
#'   \code{\link[magrittr]{set_colnames}}, \code{\link[magrittr]{set_colnames}}.
#' @export
#' @examples
#' set_names_seq(1:5)
#' set_names_seq(1:5, "X%d")
#' set_names_seq(1:5, function(i) stringr::str_c("Var_", i))
set_names_idx <- function(x, .f = "Var%d", ...) {
  set_names(x, seq_names(seq_along(x), .f, ...))
}


#' Set names in a vector by regular expression
#'
#' @param x Vector to name
#' @param pattern,replacement Pattern and replacement strings. See \code{\link[stringr]{str_replace}}
#' @param all Use \code{\link[stringr]{str_replace_all}},
#'   else use \code{\link[stringr]{str_replace}}.
#' @return Return table with renamed variables
#' @export
#' @examples
#' set_names_str(c(a = 1, b = 2), c("beta" = "b"))
#' fruits <- c("one apple" = 1, "two pears" = 2, "three bananas" = 3)
#' set_names_str(fruits, "[aeiou]", "-")
#' set_names_str(fruits, "[aeiou]", "-", all = TRUE)
replace_names <- function(x, pattern, replacement, all = TRUE) {
  assert_that(is.flag(all))
  if (all) {
    set_names_map(x, str_replace_all, pattern, replacement)
  } else {
    set_names_map(x, str_replace, pattern, replacement)
  }
}


#' Replace names in a vector
#'
#' Unlike \code{\link[purrr]{set_names}} changes a subset
#' of names.
#'
#' @param x Vector to name
#' @param nm Named character vector, where the names are the
#'   the new names and the values are the old names.
#' @return The vector \code{x} with new names.
#' @export
#' @examples
#' replace_names(c(a = 1, b = 2), c("beta" = "b"))
replace_names <- function(x, nm) {
  oldnames <- names(x)
  if (is.null(names(nm)) | any(names(nm) == "")) {
    stop("All elements of 'nm' must be named.", call. = FALSE)
  }
  bad_names <- setdiff(nm, oldnames)
  if (length(bad_names) > 0) {
    stop("Not all names in 'nm' are in 'names(x)'.",
         "Problem names: ", paste0(bad_names, collapse = ", "),
         call. = FALSE)
  }
  newnames <- oldnames
  for (i in seq_along(nm)) {
    newnames[oldnames == nm[[i]]] <- names(nm)[[i]]
  }
  set_names(x, newnames)
}

#' Map over values and names simultaneoulsy
#'
#' These functions are helper functions for common case of
#' iterating over both values and names of a vector in parallel.
#' They are equivalent to \code{map2(.x, names(.x), .f, ...)}.
#'
#' @param .x A named vector
#' @param .f A function.
#' @param ... Additional arguments passed to \code{.f}
#' @return \code{nmap} returns a list, \code{nmap_lgl} returns a logical vector,
#'   \code{nmap_dbl} a numeric vector, \code{nmap_int} an integer vector,
#'   and \code{nmap_df} a data frame.
#' @export
nmap <- function(.x, .f, ...) map2(.x, names(.x), .f, ...)

#' @export
#' @rdname nmap
nmap_lgl <- function(.x, .f, ...) map2_lgl(.x, names2(.x), .f, ...)

#' @export
#' @rdname nmap
nmap_dbl <- function(.x, .f, ...) map2_int(.x, names2(.x), .f, ...)

#' @export
#' @rdname nmap
nmap_int <- function(.x, .f, ...) map2_dbl(.x, names2(.x), .f, ...)

#' @export
#' @rdname nmap
nmap_int <- function(.x, .f, ...) map2_dbl(.x, names2(.x), .f, ...)

#' @export
#' @rdname nmap
nmap_chr <- function(.x, .f, ...) map2_chr(.x, names2(.x), .f, ...)

#' @export
#' @rdname nmap
nmap_df <- function(.x, .f, ...) map2_df(.x, names2(.x), .f, ...)

