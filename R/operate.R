#' Arithmetic operations for messydates
#'
#' These operations allow users to add or subtract dates messydate objects.
#' Messydate objects include incomplete or uncertain dates,
#' ranges of dates, negative dates, and date sets.
#' @param e1 A messydate object
#' @param e2 A numerical object
#' @return A messydates vector
#' @examples
#' d <- as_messydate(c("2008-03-25", "-2012-02-27", "2001-01?", "~2001",
#' "2001-01-01..2001-02-02", "{2001-01-01,2001-02-02}",
#' "2008-XX-31", "..2002-02-03", "2001-01-03..", "28 BC"))
#' tibble::tibble(date = d, add = d + 1, subtract = d - 1)
#' @name operate
NULL

#' @rdname operate
#' @export
`+.messydt` <- function(e1, e2){
  add(e1, e2)
}

#' @rdname operate
#' @export
`-.messydt` <- function(e1, e2){
  subtract(e1, e2)
}

#' Helper function to add to messydates
#'
#' @param x A mssydate or list of messydates
#' @param n What do you want to be added?
#' Please specify in numeric format (i.e. one day is 1).
#' @importFrom stringr str_detect str_replace str_split
#' @importFrom lubridate as_date
#' @return A messydates vector
#' @details `add()` adds date units to dates, negative dates,
#' ranges of dates, and sets of dates.
add <- function(x, n) {
  # Step one: get only first and last components for ranges
  # But keep approximation for before or after date
  x <- ifelse(stringr::str_detect(x, "^\\.\\.|\\.\\.$"), x, expand(x))
  # Step two, add by component
  x <- sapply(x, function(y) {
    if (stringr::str_detect(y[1], "^-")) {
      y <- paste0("-", lubridate::as_date(y) - n)
    } else if (stringr::str_detect(y[1], "^\\.\\.")) {
      y <- stringr::str_remove(y, "\\.\\.")
      y <- paste0("..", lubridate::as_date(y) + n)
    } else if (stringr::str_detect(y[1], "\\.\\.$")) {
      y <- stringr::str_remove(y, "\\.\\.")
      y <- paste0(lubridate::as_date(y) + n, "..")
    } else {
      y <- lubridate::as_date(y) + n
    }
    y
    })
  contract(x)
}

#' Helper function to subtract from messydates
#'
#' @param x A date or list of dates
#' @param n What do you want to be added?
#' Please specify in numeric format (i.e. one day is 1, one year is 365).
#' @importFrom stringr str_detect str_replace str_split
#' @importFrom lubridate as_date
#' @return A messydates vector
#' @details `subtract()` subtracts date units from dates, negative dates,
#' ranges of dates, and sets of dates.
#' For ranges of dates, it reduces the range by the unit on each side.
subtract <- function(x, n) {
  # Step one: get only first and last components for ranges
  # But keep approximation for before or after date
  x <- ifelse(stringr::str_detect(x, "^\\.\\.|\\.\\.$"), x, expand(x))
  # Step two, add by component
  x <- sapply(x, function(y) {
    if (stringr::str_detect(y[1], "^-")) {
      y <- paste0("-", lubridate::as_date(y) + n)
    } else if (stringr::str_detect(y[1], "^\\.\\.")) {
      y <- stringr::str_remove(y, "\\.\\.")
      y <- paste0("..", lubridate::as_date(y) - n)
    } else if (stringr::str_detect(y[1], "\\.\\.$")) {
      y <- stringr::str_remove(y, "\\.\\.")
      y <- paste0(lubridate::as_date(y) - n, "..")
    } else {
      y <- lubridate::as_date(y) - n
    }
    y
  })
  contract(x)
}
