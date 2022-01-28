#' Arithmetic operations for messydates
#'
#' @param x A date or list of dates
#' @param digit How many units?
#' @param component What date component?
#' By default year.
#' Options are year, month, or day.
#' @importFrom stringr str_detect str_replace str_split
#' @importFrom lubridate as_date
#' @return A messydates list
add <- function(x, digit, component = "year") {
  if (missing(component)) {
    stop("Please declare the date component you want to add to.
         Options are: year, month, or day.")
  }
  # Step one: get only first and last components for ranges
  x <- contract(expand(x), collapse = FALSE)
  x <- stringr::str_remove_all(x, "\\{|\\}")
  # Step two, add by component
  if (component == "year") {
    ay <- as.numeric(strsplit(as.character(digit/4), "\\.")[[1]][1]) + (365*digit)
    x <- sapply(x, function(y) {
      a <- stringr::str_split(y, "\\.\\.|\\,")[[1]][1]
      b <- stringr::str_split(y, "\\.\\.|\\,")[[1]][2]
      if (stringr::str_detect(y[1], "^-") & stringr::str_detect(y[1], "\\.\\.")) {
          y <- paste0("-", lubridate::as_date(a) + ay, "..", "-", lubridate::as_date(b) - ay)
      }
      if (stringr::str_detect(y[1], "^-") &
          stringr::str_detect(y[1], "\\.\\.", negate = TRUE)) {
        y <- paste0("-", lubridate::as_date(y) + ay)
      }
      if (stringr::str_detect(y[1], "^-", negate = TRUE) &
          stringr::str_detect(y[1], "\\.\\.")) {
        y <- paste0(lubridate::as_date(a) - ay, "..", lubridate::as_date(b) + ay)
      }
      if (stringr::str_detect(y[1], "^-", negate = TRUE) &
          stringr::str_detect(y[1], "\\.\\.", negate = TRUE) &
          stringr::str_detect(y[1], "\\,", negate = TRUE)) {
        y <- paste0(as.Date(y) + ay)
      }
      if (stringr::str_detect(y[1], "\\,")) {
        y <- paste0(lubridate::as_date(a) + ay, ",", lubridate::as_date(b) + ay)
      }
      y
    })
  }
  if (component == "month") {
    m <- 30.42 * digit
    x <- sapply(x, function(y) {
      a <- stringr::str_split(y, "\\.\\.|\\,")[[1]][1]
      b <- stringr::str_split(y, "\\.\\.|\\,")[[1]][2]
      if (stringr::str_detect(y[1], "^-") & stringr::str_detect(y[1], "\\.\\.")) {
        y <- paste0("-", lubridate::as_date(a) + m, "..", "-", lubridate::as_date(b) - m)
      }
      if (stringr::str_detect(y[1], "^-") &
          stringr::str_detect(y[1], "\\.\\.", negate = TRUE)) {
        y <- paste0("-", lubridate::as_date(y) + m)
      }
      if (stringr::str_detect(y[1], "^-", negate = TRUE) &
          stringr::str_detect(y[1], "\\.\\.")) {
        y <- paste0(lubridate::as_date(a) - m, "..", lubridate::as_date(b) + m)
      }
      if (stringr::str_detect(y[1], "^-", negate = TRUE) &
          stringr::str_detect(y[1], "\\.\\.", negate = TRUE) &
          stringr::str_detect(y[1], "\\,", negate = TRUE)) {
        y <- paste0(as.Date(y) + m)
      }
      if (stringr::str_detect(y[1], "\\,")) {
        y <- paste0(lubridate::as_date(a) + m, ",", lubridate::as_date(b) + m)
      }
      y
    })
  }
  if (component == "day") {
    d <- digit
    x <- sapply(x, function(y) {
      a <- stringr::str_split(y, "\\.\\.|\\,")[[1]][1]
      b <- stringr::str_split(y, "\\.\\.|\\,")[[1]][2]
      if (stringr::str_detect(y[1], "^-") & stringr::str_detect(y[1], "\\.\\.")) {
        y <- paste0("-", lubridate::as_date(a) + d, "..", "-", lubridate::as_date(b) - d)
      }
      if (stringr::str_detect(y[1], "^-") &
          stringr::str_detect(y[1], "\\.\\.", negate = TRUE)) {
        y <- paste0("-", lubridate::as_date(y) + d)
      }
      if (stringr::str_detect(y[1], "^-", negate = TRUE) &
          stringr::str_detect(y[1], "\\.\\.")) {
        y <- paste0(lubridate::as_date(a) - d, "..", lubridate::as_date(b) + d)
      }
      if (stringr::str_detect(y[1], "^-", negate = TRUE) &
          stringr::str_detect(y[1], "\\.\\.", negate = TRUE) &
          stringr::str_detect(y[1], "\\,", negate = TRUE)) {
        y <- paste0(as.Date(y) + d)
      }
      if (stringr::str_detect(y[1], "\\,")) {
        y <- paste0(lubridate::as_date(a) + d, ",", lubridate::as_date(b) + d)
      }
      y
    })
  }
  x <- as_messydate(x)
}
