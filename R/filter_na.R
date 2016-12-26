# http://stackoverflow.com/a/37031161/227406
filter_na <- function(.data, ...) {
  filter_na(.data, .dots = lazyeval::lazy_dots(...))
}

filter_na_ <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ...)
  if (length(dots) == 0) {
    f <- complete.cases(.data)
  } else {
    f <- complete.cases(select_(.data, .dots = dots))
  }
  filter(.data, f)
}