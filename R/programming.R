#' Check a package documentation for examples
#'
#' @param dir Root directory of a package
#' @param quiet If \code{TRUE}, then no message is displayed.
#' @return A character vector of Rd files which
#' do not have an example code. This function is primarily called
#' for its side effect of printing the list of files.
#' @export
pkg_check_examples <- function(dir, quiet = FALSE) {
  db <- tools::Rd_db(dir = dir)
  .Rd_get_example_code <- get(".Rd_get_example_code", "package:tools")
  examples <- map(db, .Rd_get_example_code)
  noex <- names(keep(examples, purrr::negate(length)))
  if (!quiet) {
    message("Rd files with no examples: \n")
    message(paste0("  - ", noex, "\n", collapse = ""))
  }
  invisible(noex)
}
