#' Check if variables are distinct
#'
#' Check if a combination of variable(s) uniquely identify
#' rows of a table.
#'
#' @param .data a tbl
#' @param ... Variables to check whether they are keys
#' @param .dots Used to work around non-standard evaluation.
#'   See vignette("nse") for details.
#' @return \code{TRUE} if the variables jointly identify all
#'   rows, and \code{FALSE} otherwise.
#' @export
is_key <- function(.data, ...) {
  is_key_(.data, .dots = lazyeval::lazy_dots(...))
}

#' @rdname is_key
#' @export
is_key_ <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ...)
  # Alternative implementation could use distinct
  # or count
  if (nrow(.data) == 0) {
    return(TRUE)
  }
  # check if any are missing
  missvars <- map_lgl(select_(.data, .dots = dots),
                      function(x) any(is.na(x)))
  if (any(missvars)) {
    return(FALSE)
  }
  # total rows
  n1 <- summarise_(ungroup(.data),
                   .dots = set_names(list(quote(n())), "n"))[["n"]]
  # number of groups
  n2 <- n_groups(group_by_(.data, .dots = dots))
  n1 == n2
}



# find_key_ <- function(.data, args, .maxp = NULL) {
#   variables <- select_vars(args)
#   p <- length(vars)
#   maxp <- .maxp %||% p
#   for (i in seq_len(maxp)) {
#     candidates <- map(cross_n(rerun(i, variables)),
#                       as.character)
#     for (cand in candidates) {
#       if (is_key_(.data, .dots = cand)) {
#         return(cand)
#       }
#     }
#   }
#   NULL
# }
#
# find_key <- function(.data, ..., .maxp = NULL) {
#   args <- lazyeval::lazy_dots(...)
#   find_key_(.data, args, .maxp = .maxp)
# }
#
# find_keys_ <- function(.data, args, .maxp = 1) {
#   if (length(args) == 0) {
#     args <- quote(everything())
#   }
#   variables <- select_vars_(names(.data), args)
#   p <- length(vars)
#   maxp <- .maxp %||% p
#   removedups <- function(...) any(duplicated(list(...)))
#   fun <- function(i, variables) {
#     cross_n(map(seq_len(i), variables), removedups)
#   }
#   candidates <- map(flatten(map(seq_len(maxp), fun, variables = variables)),
#                     as.character)
#   keep(candidates, function(x, .data) is_key_(.data, .dots = x),
#        .data = .data)
# }
#
# find_keys <- function(.data, ..., .maxp = 1) {
#   args <- lazyeval::lazy_dots(...)
#   find_keys_(.data, args, .maxp = .maxp)
# }
