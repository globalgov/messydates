#' Extracting components from messy dates
#' @description
#'   These functions allow the extraction of particular date components
#'   from messy dates, such as the `year()`, `month()`, `day()`, and, for
#'   date-times, `hour()`, `minute()`, `second()`, and the time zone (`tz()`).
#'
#'   These are methods for the same-named generics in `{lubridate}`, so they
#'   extend rather than mask them: calling e.g. `year()` on an `mdate` returns
#'   the messy-date-aware result (understanding partial precision such as
#'   `2012-06-XX`), while calling it on a `Date` or `POSIXct` still dispatches
#'   to `{lubridate}`'s own methods. This lets `{messydates}` and `{lubridate}`
#'   be loaded together, in either order, without one masking the other.
#'
#'   `precision()` allows for the identification of the greatest level of
#'   precision in (currently) the first element of each date.
#' @param x A `mdate` object
#' @param ... Additional arguments passed to or from other methods
#'   (accepted for compatibility with the `{lubridate}` generics; unused).
#' @return `year()`, `month()`, `day()`, `hour()`, `minute()`, and `second()`
#'   extraction return the integer for the requested component (`NA` where the
#'   component is absent or unspecified).
#'
#'   `tz()` returns the time zone designator or offset as a string.
#'
#'   `precision()` returns the level of greatest precision for each date.
#' @name component_extract
NULL

# Re-export the {lubridate} accessor generics so that {messydates}' methods
# below extend them (rather than masking them with plain functions).

#' @importFrom lubridate year
#' @export
lubridate::year

#' @importFrom lubridate month
#' @export
lubridate::month

#' @importFrom lubridate day
#' @export
lubridate::day

# {lubridate}'s day() dispatches via UseMethod("mday"), so the method below is
# registered on mday() (day() then finds it too).
#' @importFrom lubridate mday
#' @export
lubridate::mday

#' @importFrom lubridate hour
#' @export
lubridate::hour

#' @importFrom lubridate minute
#' @export
lubridate::minute

#' @importFrom lubridate second
#' @export
lubridate::second

#' @importFrom lubridate tz
#' @export
lubridate::tz

#' @rdname component_extract
#' @examples
#' year(as_messydate(c("2012-02-03","2012","2012-02")))
#' @exportS3Method lubridate::year
year.mdate <- function(x, ...) {
  x <- as.character(x)
  x <- stringi::stri_replace_all_regex(x, "\\.\\..+", "")
  x <- stringi::stri_replace_all_regex(x, "-.+", "")
  # A bare time ("14:30") has no year component; return NA rather than warning.
  x[stringi::stri_detect_regex(x, "^[~?%]?[0-9X]{1,2}:")] <- NA_character_
  suppressWarnings(as.integer(x))
}

#' @rdname component_extract
#' @examples
#' month(as_messydate(c("2012-02-03","2012","2012-02")))
#' @exportS3Method lubridate::month
month.mdate <- function(x, ...) {
  m <- stringi::stri_match_first_regex(as.character(x),
                                       "^-?[0-9X]{4}-([0-9X]{1,2})")[, 2]
  suppressWarnings(as.integer(m))
}

#' @rdname component_extract
#' @examples
#' day(as_messydate(c("2012-02-03","2012","2012-02")))
#' @exportS3Method lubridate::mday
mday.mdate <- function(x, ...) {
  d <- stringi::stri_match_first_regex(
    as.character(x), "^-?[0-9X]{4}-[0-9X]{1,2}-([0-9X]{1,2})")[, 2]
  suppressWarnings(as.integer(d))
}

# The shared time regexes anchor on the '[T ]' date-time separator. A bare
# time (no date part, e.g. "14:30") has no such separator, so prefix a space
# to those elements, letting the same regexes extract their time components.
mark_bare_time <- function(x) {
  x <- as.character(x)
  bare <- stringi::stri_detect_regex(x, "^[~?%]?[0-9X]{1,2}:")
  bare[is.na(bare)] <- FALSE
  x[bare] <- paste0(" ", x[bare])
  x
}

#' @rdname component_extract
#' @examples
#' hour(as_messydate(c("2012-02-03 14:30:00","2012-02-03")))
#' @exportS3Method lubridate::hour
hour.mdate <- function(x, ...) {
  h <- stringi::stri_match_first_regex(mark_bare_time(x), "[T ][~?%]?([0-9X]{2})")[, 2]
  suppressWarnings(as.integer(h))
}

#' @rdname component_extract
#' @examples
#' minute(as_messydate("2012-02-03 14:30:00"))
#' @exportS3Method lubridate::minute
minute.mdate <- function(x, ...) {
  m <- stringi::stri_match_first_regex(
    mark_bare_time(x), "[T ][~?%]?[0-9X]{2}:[~?%]?([0-9X]{2})"
  )[, 2]
  suppressWarnings(as.integer(m))
}

#' @rdname component_extract
#' @examples
#' second(as_messydate("2012-02-03 14:30:05"))
#' @exportS3Method lubridate::second
second.mdate <- function(x, ...) {
  s <- stringi::stri_match_first_regex(
    mark_bare_time(x),
    "[T ][~?%]?[0-9X]{2}:[~?%]?[0-9X]{2}:[~?%]?([0-9X]{2}(?:\\.[0-9]+)?)"
  )[, 2]
  suppressWarnings(as.numeric(s))
}

#' @rdname component_extract
#' @details
#'   Unlike `{lubridate}`'s `tz()`, which returns an Olson time zone name,
#'   `tz.mdate()` returns the ISO 8601 UTC offset *designator* carried by the
#'   date-time string (`"Z"` or e.g. `"+02:00"`), or `NA` when none is present.
#' @examples
#' tz(as_messydate("2012-02-03 14:30:00+02:00"))
#' @exportS3Method lubridate::tz
tz.mdate <- function(x, ...) {
  stringi::stri_match_first_regex(mark_bare_time(x),
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
#' precision(as_messydate("2012-02-03 14:30"))
#' @export
precision.mdate <- function(x) {
  out <- expand(x)
  (1 / lengths(out)) * subday_factor(x)
}

# Multiplier extending the 1/days precision scale below the day: 24 for hour,
# 1440 for minute, 86400 for second precision; 1 when no time is present.
subday_factor <- function(x) {
  x <- mark_bare_time(x)
  f <- rep(1, length(x))
  f[stringi::stri_detect_regex(x, "[T ][0-9X]{2}")] <- 24
  f[stringi::stri_detect_regex(x, "[T ][0-9X]{2}:[0-9X]{2}")] <- 1440
  f[stringi::stri_detect_regex(x, "[T ][0-9X]{2}:[0-9X]{2}:[0-9X]{2}")] <- 86400
  f
}
