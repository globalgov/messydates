#' Extracting components from messy dates
#' @description
#'   These functions allow the extraction of particular date components
#'   from messy dates, such as the `year()`, `month()`, `day()`, and, for
#'   date-times, `hour()`, `minute()`, `second()`, and the time zone (`tz()`).
#'   `precision()` allows for the identification of the greatest level of
#'   precision in (currently) the first element of each date.
#' @param x A `mdate` object
#' @return `year()`, `month()`, `day()`, `hour()`, `minute()`, and `second()`
#'   extraction return the integer for the requested component (`NA` where the
#'   component is absent or unspecified). `tz()` returns the time zone
#'   designator or offset as a string.
#'   `precision()` returns the level of greatest precision for each date.
#' @name component_extract
NULL
#> NULL

#' @rdname component_extract
#' @examples
#' year(as_messydate(c("2012-02-03","2012","2012-02")))
#' @export
year <- function(x) {
  x <- stringi::stri_replace_all_regex(x, "\\.\\..+", "")
  x <- stringi::stri_replace_all_regex(x, "-.+", "")
  as.integer(x)
}

#' @rdname component_extract
#' @examples
#' month(as_messydate(c("2012-02-03","2012","2012-02")))
#' @export
month <- function(x) {
  m <- stringi::stri_match_first_regex(as.character(x),
                                       "^-?[0-9X]{4}-([0-9X]{1,2})")[, 2]
  suppressWarnings(as.integer(m))
}

#' @rdname component_extract
#' @examples
#' day(as_messydate(c("2012-02-03","2012","2012-02")))
#' @export
day <- function(x) {
  d <- stringi::stri_match_first_regex(
    as.character(x), "^-?[0-9X]{4}-[0-9X]{1,2}-([0-9X]{1,2})")[, 2]
  suppressWarnings(as.integer(d))
}

#' @rdname component_extract
#' @examples
#' hour(as_messydate(c("2012-02-03T14:30:00","2012-02-03")))
#' @export
hour <- function(x) {
  h <- stringi::stri_match_first_regex(as.character(x), "T[~?%]?([0-9X]{2})")[, 2]
  suppressWarnings(as.integer(h))
}

#' @rdname component_extract
#' @examples
#' minute(as_messydate("2012-02-03T14:30:00"))
#' @export
minute <- function(x) {
  m <- stringi::stri_match_first_regex(
    as.character(x), "T[~?%]?[0-9X]{2}:[~?%]?([0-9X]{2})"
  )[, 2]
  suppressWarnings(as.integer(m))
}

#' @rdname component_extract
#' @examples
#' second(as_messydate("2012-02-03T14:30:05"))
#' @export
second <- function(x) {
  s <- stringi::stri_match_first_regex(
    as.character(x),
    "T[~?%]?[0-9X]{2}:[~?%]?[0-9X]{2}:[~?%]?([0-9X]{2}(?:\\.[0-9]+)?)"
  )[, 2]
  suppressWarnings(as.numeric(s))
}

#' @rdname component_extract
#' @examples
#' tz(as_messydate("2012-02-03T14:30:00+02:00"))
#' @export
tz <- function(x) {
  stringi::stri_match_first_regex(as.character(x),
                                  "(Z|[+-][0-9]{2}:[0-9]{2})$")[, 2]
}

#' @rdname component_extract
#' @export
precision <- function(x) UseMethod("precision")

#' @rdname component_extract
#' @section Precision:
#'   Date precision is measured relative to the day in \eqn{1/days(x)}.
#'   That is, a date measured to the day will return a precision score
#'   of 1, a date measured to the month will return a precision score of
#'   between \eqn{1/28} and \eqn{1/31}, and annual measures will have
#'   a precision of between \eqn{1/365} and \eqn{1/366}.
#'   Times of day extend the same scale below the day: a date-time measured
#'   to the hour returns 24, to the minute 1440, and to the second 86400.
#' @examples
#' precision(as_messydate(c("2012-02-03","2012","2012-02")))
#' precision(as_messydate("2012-02-03T14:30"))
#' @export
precision.mdate <- function(x) {
  out <- expand(x)
  (1 / lengths(out)) * subday_factor(x)
}

# Multiplier extending the 1/days precision scale below the day: 24 for hour,
# 1440 for minute, 86400 for second precision; 1 when no time is present.
subday_factor <- function(x) {
  x <- as.character(x)
  f <- rep(1, length(x))
  f[stringi::stri_detect_regex(x, "T[0-9X]{2}")] <- 24
  f[stringi::stri_detect_regex(x, "T[0-9X]{2}:[0-9X]{2}")] <- 1440
  f[stringi::stri_detect_regex(x, "T[0-9X]{2}:[0-9X]{2}:[0-9X]{2}")] <- 86400
  f
}
