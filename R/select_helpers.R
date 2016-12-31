#' Select helper to select variables based on name glob pattern.
#'
#' Like \code{\link[dplyr]{matches}}, this function selects variables by by name.
#' However, it uses wildcard, aka globbing patterns instead of regular expressions.
#'
#' @details
#'
#' Use \code{?} to match a single character, and \code{*} to match any number of
#' characters, including none. The pattern is anchored at the start and end of the
#' string, meaning it must start at the start, and end at the the end.
#'
#' This function uses \code{\link[utils]{glob2rx}} to convert the globbing pattern
#' to a regex,
#'
#' @param pattern Globbing pattern
#' @param ignore.case If \code{TRUE}, the default, ignores case when matching names.
#' @param vars A character vector of variable names.
#'    When called from inside \code{select()} these are automatically set
#'    to the names of the table.
#' @return An integer vector given the position of the matched variables.
#' @export
glob <- function(pattern, ignore.case = TRUE, vars = current_vars()) {
  # no reason to drop or not  trim.head, trim.tail. it only simplifies
  # the regex, doesn't change behavior.
  matches(glob2rx(pattern), ignore.case = ignore.case, vars = vars)
}
