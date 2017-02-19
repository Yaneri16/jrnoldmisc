# recommended imports
#' @importFrom stats contr.treatment
#' @importFrom stats complete.cases
#' @importFrom stats model.matrix
#' @importFrom utils glob2rx
#'
#' @importFrom assertthat is.flag
#' @importFrom assertthat "on_failure<-"
#' @importFrom dplyr arrange_
#' @importFrom dplyr count_
#' @importFrom dplyr current_vars
#' @importFrom dplyr desc
#' @importFrom dplyr distinct_
#' @importFrom dplyr everything
#' @importFrom dplyr filter
#' @importFrom dplyr filter_
#' @importFrom dplyr groups
#' @importFrom dplyr group_by_
#' @importFrom dplyr matches
#' @importFrom dplyr mutate_
#' @importFrom dplyr n_groups
#' @importFrom dplyr rename_
#' @importFrom dplyr select_
#' @importFrom dplyr select_vars_
#' @importFrom dplyr summarise_
#' @importFrom dplyr tbl
#' @importFrom dplyr tbl_vars
#' @importFrom dplyr ungroup
#' @importFrom ggplot2 scale_x_continuous
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom lazyeval lazy_dots
#' @importFrom purrr flatten
#' @importFrom purrr cross_n
#' @importFrom purrr keep
#' @importFrom purrr cross_n
#' @importFrom purrr "%||%"
#' @importFrom purrr set_names
#' @importFrom purrr as_function
#' @importFrom purrr map
#' @importFrom purrr map_lgl
#' @importFrom purrr rerun
#' @importFrom purrr reduce
#' @importFrom purrr map_df
#' @importFrom purrr map2
#' @importFrom purrr map2_df
#' @importFrom purrr map2_lgl
#' @importFrom purrr map2_dbl
#' @importFrom purrr map2_chr
#' @importFrom purrr map2_int
#' @importFrom purrr is_function
#' @importFrom purrr is_character
#' @importFrom stringr str_replace
#' @importFrom stringr str_replace_all
#' @importFrom purrr is_numeric
#' @importFrom forcats lvls_revalue
NULL

#' Test for equality with NA, NaN
#'
#' Like \code{x == y}, but not returning \code{NA} in tests involving
#' \code{NaN} or \code{NA}.
#'
#' @param x,y Values to test
#' @return A logical vector with the test results.
#' @examples
#' x <- c(1, NA)
#' y <- c(1, NA)
#'
#' @export
#' @examples
#' x <- c(0, 1, NA, NaN, NA, NaN)
#' y <- c(0, 0, NA, NaN, 0, 0)
#' x %==% y
"%==%" <- function(x, y) {
  z <- (is.na(x) & is.na(y)) | (is.nan(x) & is.nan(y)) | x == y
  z[is.na(z)] <- FALSE
  z
}
