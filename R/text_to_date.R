#' Extract dates from text
#'
#' Sometimes dates can be contained in text,
#' this function extracts those dates from text and
#' converts them to messydt objects.
#' @param v A character vector
#' @return Dates in messydt format
#' @importFrom stringr str_replace_all str_squish str_extract word str_starts
#' str_remove_all
#' @details If multiple dates are identified in the same row,
#' only the first date is returned.
#' @examples
#' v <- c("This function was created on the 29 September 2021",
#' "Today is October 12, 2021", "Tomorrow is 13-10-2021")
#' text_to_date(v)
#' @export
text_to_date <- function(v) {
  # make all lower case
  out <- tolower(v)
  # remove commas
  out <- gsub(",", "", out)
  # remove ordinal signs and date related articles
  out <- gsub("de |of |st |nd |rd |th ", " ", out)
  # correct double white space left
  out <- stringr::str_squish(out)
  # get the first date per row
  out <- extract_from_text(out)
  # re-order dates if necessary
  out <- lapply(out, re_order)
  # get the months into numeric form
  months <- data.frame(months = c("january", "february", "march", "april",
                                  "may", "june", "july", "august", "september",
                                  "october", "november", "december"),
                       number = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))
  for (k in seq_len(nrow(months))) {
    out <- gsub(paste0(months$months[k]),
                paste0(months$number[k]),
                out, ignore.case = TRUE,
                perl = T)
  }
  # standardize separators
  out <- stringr::str_replace_all(out, " |/", "-")
  out <- stringr::str_replace_all(out, "[a-z]|[A-Z]", "?")
  out <- as_messydate(ifelse(out == "", NA, out))
  out
}

extract_from_text <- function(x) {
  out <- stringr::str_extract(x,
                              "[:digit:]{2}\\s[:alpha:]{3}\\s[:digit:]{4}|
                              |[:digit:]{2}\\s[:alpha:]{4}\\s[:digit:]{4}|
                              |[:digit:]{2}\\s[:alpha:]{5}\\s[:digit:]{4}|
                              |[:digit:]{2}\\s[:alpha:]{6}\\s[:digit:]{4}|
                              |[:digit:]{2}\\s[:alpha:]{7}\\s[:digit:]{4}|
                              |[:digit:]{2}\\s[:alpha:]{8}\\s[:digit:]{4}|
                              |[:digit:]{2}\\s[:alpha:]{9}\\s[:digit:]{4}|
                              |[:digit:]{1}\\s[:alpha:]{3}\\s[:digit:]{4}|
                              |[:digit:]{1}\\s[:alpha:]{4}\\s[:digit:]{4}|
                              |[:digit:]{1}\\s[:alpha:]{5}\\s[:digit:]{4}|
                              |[:digit:]{1}\\s[:alpha:]{6}\\s[:digit:]{4}|
                              |[:digit:]{1}\\s[:alpha:]{7}\\s[:digit:]{4}|
                              |[:digit:]{1}\\s[:alpha:]{8}\\s[:digit:]{4}|
                              |[:digit:]{2}\\s[:alpha:]{9}\\s[:digit:]{4}|
                              |[:alpha:]{3}\\s[:digit:]{2}\\s[:digit:]{4}|
                              |[:alpha:]{4}\\s[:digit:]{2}\\s[:digit:]{4}|
                              |[:alpha:]{5}\\s[:digit:]{2}\\s[:digit:]{4}|
                              |[:alpha:]{6}\\s[:digit:]{2}\\s[:digit:]{4}|
                              |[:alpha:]{7}\\s[:digit:]{2}\\s[:digit:]{4}|
                              |[:alpha:]{8}\\s[:digit:]{2}\\s[:digit:]{4}|
                              |[:alpha:]{9}\\s[:digit:]{2}\\s[:digit:]{4}|
                              |[:alpha:]{3}\\s[:digit:]{1}\\s[:digit:]{4}|
                              |[:alpha:]{4}\\s[:digit:]{1}\\s[:digit:]{4}|
                              |[:alpha:]{5}\\s[:digit:]{1}\\s[:digit:]{4}|
                              |[:alpha:]{6}\\s[:digit:]{1}\\s[:digit:]{4}|
                              |[:alpha:]{7}\\s[:digit:]{1}\\s[:digit:]{4}|
                              |[:alpha:]{8}\\s[:digit:]{1}\\s[:digit:]{4}|
                              |[:alpha:]{9}\\s[:digit:]{1}\\s[:digit:]{4}|
                              |[:digit:]{2}-[:digit:]{2}-[:digit:]{4}|
                              |[:digit:]{1}-[:digit:]{2}-[:digit:]{4}|
                              |[:digit:]{2}-[:digit:]{2}-[:digit:]{2}|
                              |[:digit:]{1}-[:digit:]{2}-[:digit:]{2}|
                              |[:digit:]{2}-[:digit:]{1}-[:digit:]{4}|
                              |[:digit:]{1}-[:digit:]{1}-[:digit:]{4}|
                              |[:digit:]{2}-[:digit:]{1}-[:digit:]{2}|
                              |[:digit:]{1}-[:digit:]{1}-[:digit:]{2}|
                              |[:digit:]{4}-[:digit:]{2}-[:digit:]{2}|
                              |[:digit:]{4}-[:digit:]{2}-[:digit:]{1}|
                              |[:digit:]{4}-[:digit:]{1}-[:digit:]{2}|
                              |[:digit:]{4}-[:digit:]{1}-[:digit:]{1}|
                              |[:digit:]{2}/[:digit:]{2}/[:digit:]{4}|
                              |[:digit:]{1}/[:digit:]{2}/[:digit:]{4}|
                              |[:digit:]{2}/[:digit:]{2}/[:digit:]{2}|
                              |[:digit:]{1}/[:digit:]{2}/[:digit:]{2}|
                              |[:digit:]{2}/[:digit:]{1}/[:digit:]{4}|
                              |[:digit:]{1}/[:digit:]{1}/[:digit:]{4}|
                              |[:digit:]{2}/[:digit:]{1}/[:digit:]{2}|
                              |[:digit:]{1}/[:digit:]{1}/[:digit:]{2}|
                              |[:digit:]{4}/[:digit:]{2}/[:digit:]{2}|
                              |[:digit:]{4}/[:digit:]{2}/[:digit:]{1}|
                              |[:digit:]{4}/[:digit:]{1}/[:digit:]{2}|
                              |[:digit:]{4}/[:digit:]{1}/[:digit:]{1}|
                              |[:digit:]{3}\\s[:alpha:]{4}\\s[:digit:]{2}|
                              |[:digit:]{4}\\s[:alpha:]{4}\\s[:digit:]{2}|
                              |[:digit:]{4}\\s[:alpha:]{5}\\s[:digit:]{2}|
                              |[:digit:]{4}\\s[:alpha:]{5}\\s[:digit:]{2}|
                              |[:digit:]{4}\\s[:alpha:]{6}\\s[:digit:]{2}|
                              |[:digit:]{4}\\s[:alpha:]{7}\\s[:digit:]{2}|
                              |[:digit:]{4}\\s[:alpha:]{8}\\s[:digit:]{2}|
                              |[:digit:]{4}\\s[:alpha:]{9}\\s[:digit:]{2}|
                              |[:digit:]{3}\\s[:alpha:]{4}\\s[:digit:]{1}|
                              |[:digit:]{4}\\s[:alpha:]{4}\\s[:digit:]{1}|
                              |[:digit:]{4}\\s[:alpha:]{5}\\s[:digit:]{1}|
                              |[:digit:]{4}\\s[:alpha:]{5}\\s[:digit:]{1}|
                              |[:digit:]{4}\\s[:alpha:]{6}\\s[:digit:]{1}|
                              |[:digit:]{4}\\s[:alpha:]{7}\\s[:digit:]{1}|
                              |[:digit:]{4}\\s[:alpha:]{8}\\s[:digit:]{1}|
                              |[:digit:]{4}\\s[:alpha:]{9}\\s[:digit:]{1}")
  out
}

re_order <- function(l) {
  l <- stringr::str_squish(l)
  st <- stringr::word(l, 1)
  mi <- stringr::word(l, 2)
  ed <- stringr::word(l, 3)
  out <- ifelse(stringr::str_starts(l, "[:digit:]{4}"),
                paste0(ed, "-", mi, "-", st), l)
  out <- ifelse(stringr::str_starts(out, "[:alpha:]"),
                paste0(mi, "-", st, "-", ed), out)
  out <- stringr::str_remove_all(out, "-NA|NA-|NA")
  out
}
