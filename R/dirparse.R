library("purrr")
library("tidyverse")

parse_data_dir <- function(path = ".") {
  filenames <- dir(path, pattern = "\\.[rR]$", full.names = TRUE)
  map_df(filenames, get_parse_file) %>%
    count(token, text) %>%
    arrange(desc(nn))
}

get_parse_file <- function(filename,
                           tokens = c("SYMBOL_FUNCTION_CALL", "SPECIAL")) {
  getParseData(parse(filename, keep.source = TRUE)) %>%
    select(token, text) %>%
    dplyr::filter(token %in% tokens) %>%
    group_by(token, text) %>%
    tally() %>%
    mutate(filename = filename)
}

