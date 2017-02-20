#' Deaccent a string
#'
#' Remove accents from a string
#'
#' @param x Character vector
#' @return Character vector
#' @export
#' @importFrom stringi stri_trans_nfd
#' @importFrom stringi stri_replace_all_charclass
#' @importFrom stringi stri_trans_nfc
deaccent <- function(x) {
  x <- iconv(x, to = "UTF-8")
  x <- stri_trans_nfd(x)
  x <- stri_replace_all_charclass(x, "\\p{Mn}", "")
  x <- stri_trans_nfc(x)
  x
}