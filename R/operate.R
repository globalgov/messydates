#' Arithmetic operations for messydates
#'
#' These operations allow users to add or subtract dates messydate objects.
#' Messydate objects include imcomplete or uncertain dates,
#' ranges of dates, negative dates, and date sets.
#' @param x A date or list of dates
#' @param digit How many date units?
#' @param component What date component?
#' By default year.
#' Other options are month and day.
#' @importFrom stringr str_detect str_replace str_split
#' @importFrom lubridate as_date
#' @return A messydates list
#' @examples
#' d <- as_messydate(c("2008-03-25", "-2012-02-27", "2001-01?", "~2001",
#' "2001-01-01..2001-02-02", "{2001-01-01,2001-02-02}",
#' "2008-XX-31", "..2002-02-03", "2001-01-03..", "28 BC"))
#' tibble::tibble(d, add(d, 3), subtract(d, 3))
#' @name operate

#' @rdname operate
#' @details `add()` adds date units to dates, negative dates,
#' ranges of dates, and sets of dates.
#' For ranges of dates, it increases the range by the unit on each side.
#' @export
add <- function(x, digit, component = "year") {
  # Step one: get only first and last components for ranges
  # But keep approximation for before or after date
  x <- ifelse(stringr::str_detect(x, "^\\.\\.|\\.\\.$"), x,
              contract(expand(x), collapse = FALSE))
  x <- stringr::str_remove_all(x, "\\{|\\}")
  # Step two, add by component
  if (component == "year") {
    ay <- as.numeric(strsplit(as.character(digit/4), "\\.")[[1]][1]) + (365*digit)
    x <- sapply(x, function(y) {
      a <- stringr::str_split(y, "\\.\\.")[[1]][1]
      b <- stringr::str_split(y, "\\.\\.")[[1]][2]
      if (stringr::str_detect(y[1], "[:digit:]{2}\\.\\.\\-[:digit:]{4}")) {
          y <- paste0("-", lubridate::as_date(a) + ay, "..", "-", lubridate::as_date(b) - ay)
      }
      if (stringr::str_detect(y[1], "^-") &
          stringr::str_detect(y[1], "\\.\\.", negate = TRUE)) {
        y <- paste0("-", lubridate::as_date(y) + ay)
      }
      if (stringr::str_detect(y[1], "[:digit:]{2}\\.\\.[:digit:]{4}")) {
        y <- paste0(lubridate::as_date(a) - ay, "..", lubridate::as_date(b) + ay)
      }
      if (stringr::str_detect(y[1], "^-", negate = TRUE) &
          stringr::str_detect(y[1], "\\.\\.", negate = TRUE) &
          stringr::str_detect(y[1], "\\,", negate = TRUE)) {
        y <- paste0(lubridate::as_date(y) + ay)
      }
      if (stringr::str_detect(y[1], "\\,")) {
        y <- stringr::str_split(y, "\\,")
        y <- lapply(y, function(s) {
          as.character(lubridate::as_date(s) + ay)
        })
        y <- paste(y, sep = ",")
      }
      if (stringr::str_detect(y[1], "^\\.\\.")) {
        y <- stringr::str_remove(y, "\\.\\.")
        y <- paste0("..", lubridate::as_date(y) + ay)
      }
      if (stringr::str_detect(y[1], "\\.\\.$")) {
        y <- stringr::str_remove(y, "\\.\\.")
        y <- paste0(lubridate::as_date(y) + ay, "..")
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
        y <- stringr::str_split(y, "\\,")
        y <- lapply(y, function(s) {
          as.character(lubridate::as_date(s) + m)
        })
        y <- paste(y, sep = ",")
      }
      if (stringr::str_detect(y[1], "^\\.\\.")) {
        y <- stringr::str_remove(y, "\\.\\.")
        y <- paste0("..", lubridate::as_date(y) + m)
      }
      if (stringr::str_detect(y[1], "\\.\\.$")) {
        y <- stringr::str_remove(y, "\\.\\.")
        y <- paste0(lubridate::as_date(y) + m, "..")
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
        y <- stringr::str_split(y, "\\,")
        y <- lapply(y, function(s) {
          as.character(lubridate::as_date(s) + d)
        })
        y <- paste(y, sep = ",")
      }
      if (stringr::str_detect(y[1], "^\\.\\.")) {
        y <- stringr::str_remove(y, "\\.\\.")
        y <- paste0("..", lubridate::as_date(y) + d)
      }
      if (stringr::str_detect(y[1], "\\.\\.$")) {
        y <- stringr::str_remove(y, "\\.\\.")
        y <- paste0(lubridate::as_date(y) + d, "..")
      }
      y
    })
  }
  x <- ifelse(stringr::str_detect(x, ","), paste0("{", x, "}"), x)
  x <- stringr::str_remove_all(x, "c\\(|\\)")
  x <- stringr::str_remove_all(x, '"')
  as_messydate(x)
}

#' @rdname operate
#' @details `subtract()` subtracts date units from dates, negative dates,
#' ranges of dates, and sets of dates.
#' For ranges of dates, it reduces the range by the unit on each side.
#' @export
subtract <- function(x, digit, component = "year") {
  # Step one: get only first and last components for ranges
  x <- ifelse(stringr::str_detect(x, "^\\.\\.|\\.\\.$"), x,
              contract(expand(x), collapse = FALSE))
  x <- stringr::str_remove_all(x, "\\{|\\}")
  # Step two, subtract by component
  if (component == "year") {
    ay <- as.numeric(strsplit(as.character(digit/4), "\\.")[[1]][1]) + (365*digit)
    x <- sapply(x, function(y) {
      a <- stringr::str_split(y, "\\.\\.")[[1]][1]
      b <- stringr::str_split(y, "\\.\\.")[[1]][2]
      if (stringr::str_detect(y[1], "[:digit:]{2}\\.\\.\\-[:digit:]{4}")) {
        y <- paste0("-", lubridate::as_date(a) - ay, "..", "-", lubridate::as_date(b) + ay)
      }
      if (stringr::str_detect(y[1], "^-") &
          stringr::str_detect(y[1], "\\.\\.", negate = TRUE)) {
        y <- paste0("-", lubridate::as_date(y) - ay)
      }
      if (stringr::str_detect(y[1], "[:digit:]{2}\\.\\.[:digit:]{4}")) {
        y <- paste0(lubridate::as_date(a) + ay, "..", lubridate::as_date(b) - ay)
      }
      if (stringr::str_detect(y[1], "^-", negate = TRUE) &
          stringr::str_detect(y[1], "\\.\\.", negate = TRUE) &
          stringr::str_detect(y[1], "\\,", negate = TRUE)) {
        y <- paste0(lubridate::as_date(y) - ay)
      }
      if (stringr::str_detect(y[1], "\\,")) {
        y <- stringr::str_split(y, "\\,")
        y <- lapply(y, function(s) {
          as.character(lubridate::as_date(s) - ay)
        })
        y <- paste(y, sep = ",")
      }
      if (stringr::str_detect(y[1], "^\\.\\.")) {
        y <- stringr::str_remove(y, "\\.\\.")
        y <- paste0("..", lubridate::as_date(y) - ay)
      }
      if (stringr::str_detect(y[1], "\\.\\.$")) {
        y <- stringr::str_remove(y, "\\.\\.")
        y <- paste0(lubridate::as_date(y) - ay, "..")
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
        y <- paste0("-", lubridate::as_date(a) - m, "..", "-", lubridate::as_date(b) + m)
      }
      if (stringr::str_detect(y[1], "^-") &
          stringr::str_detect(y[1], "\\.\\.", negate = TRUE)) {
        y <- paste0("-", lubridate::as_date(y) - m)
      }
      if (stringr::str_detect(y[1], "^-", negate = TRUE) &
          stringr::str_detect(y[1], "\\.\\.")) {
        y <- paste0(lubridate::as_date(a) + m, "..", lubridate::as_date(b) - m)
      }
      if (stringr::str_detect(y[1], "^-", negate = TRUE) &
          stringr::str_detect(y[1], "\\.\\.", negate = TRUE) &
          stringr::str_detect(y[1], "\\,", negate = TRUE)) {
        y <- paste0(lubridate::as_date(y) - m)
      }
      if (stringr::str_detect(y[1], "\\,")) {
        y <- stringr::str_split(y, "\\,")
        y <- lapply(y, function(s) {
          as.character(lubridate::as_date(s) - m)
        })
        y <- paste(y, sep = ",")
      }
      if (stringr::str_detect(y[1], "^\\.\\.")) {
        y <- stringr::str_remove(y, "\\.\\.")
        y <- paste0("..", lubridate::as_date(y) - m)
      }
      if (stringr::str_detect(y[1], "\\.\\.$")) {
        y <- stringr::str_remove(y, "\\.\\.")
        y <- paste0(lubridate::as_date(y) - m, "..")
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
        y <- paste0("-", lubridate::as_date(a) - d, "..", "-", lubridate::as_date(b) + d)
      }
      if (stringr::str_detect(y[1], "^-") &
          stringr::str_detect(y[1], "\\.\\.", negate = TRUE)) {
        y <- paste0("-", lubridate::as_date(y) - d)
      }
      if (stringr::str_detect(y[1], "^-", negate = TRUE) &
          stringr::str_detect(y[1], "\\.\\.")) {
        y <- paste0(lubridate::as_date(a) + d, "..", lubridate::as_date(b) - d)
      }
      if (stringr::str_detect(y[1], "^-", negate = TRUE) &
          stringr::str_detect(y[1], "\\.\\.", negate = TRUE) &
          stringr::str_detect(y[1], "\\,", negate = TRUE)) {
        y <- paste0(lubridate::as_date(y) - d)
      }
      if (stringr::str_detect(y[1], "\\,")) {
        y <- stringr::str_split(y, "\\,")
        y <- lapply(y, function(s) {
          as.character(lubridate::as_date(s) - d)
        })
        y <- paste(y, sep = ",")
      }
      if (stringr::str_detect(y[1], "^\\.\\.")) {
        y <- stringr::str_remove(y, "\\.\\.")
        y <- paste0("..", lubridate::as_date(y) - d)
      }
      if (stringr::str_detect(y[1], "\\.\\.$")) {
        y <- stringr::str_remove(y, "\\.\\.")
        y <- paste0(lubridate::as_date(y) - d, "..")
      }
      y
    })
  }
  x <- ifelse(stringr::str_detect(x, ","), paste0("{", x, "}"), x)
  x <- stringr::str_remove_all(x, "c\\(|\\)")
  x <- stringr::str_remove_all(x, '"')
  as_messydate(x)
}
