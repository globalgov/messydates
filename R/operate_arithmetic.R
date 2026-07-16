#' Arithmetic operations for messydates
#'
#' These operations allow users to add or subtract dates messydate objects.
#' Messydate objects include incomplete or uncertain dates,
#' ranges of dates, negative dates, and date sets.
#' @details
#'   When `e2` is a number or a string naming a unit ("day", "week",
#'   "month", "year", or, for date-times, "hour", "min"/"minute", or
#'   "sec"/"second"), `e1` is shifted by that amount:
#'   \itemize{
#'    \item{A bare number, or a "day"/"week" unit, shifts by that many
#'    days.}
#'    \item{"month" and "year" shift the calendar component itself
#'    (preserving the day of month and any time of day), rolling back to
#'    the last valid day where necessary, e.g. adding a month to
#'    `"2012-01-31"` gives `"2012-02-29"` (2012 being a leap year), not an
#'    invalid `"2012-02-31"`.}
#'    \item{Sub-day units, or any unit at all when `e1` carries a time of
#'    day, shift the instant in seconds via `POSIXct`, promoting a
#'    date-only `e1` to a time if the shift is sub-day.}
#'   }
#'   When `e2` is itself an `mdate`, `+`/`-` instead treat both sides as
#'   sets of dates: `+` returns their union (as a multiset, in the most
#'   succinct `mdate` notation; see also `?operate_set` for `%union%`, which
#'   returns a plain vector of the member dates instead), and `-` removes
#'   the dates in `e2` from `e1`.
#' @param e1 An `mdate` or date object.
#' @param e2 An `mdate`, date, or numeric object. Must be a scalar.
#' @return A messydates vector
#' @examples
#' \donttest{
#' d <- as_messydate(c("2008-03-25", "-2012-02-27", "2001-01?", "~2001",
#' "2001-01-01..2001-02-02", "{2001-01-01,2001-02-02}",
#' "2008-XX-31", "..2002-02-03", "2001-01-03..", "28 BC"))
#' data.frame(date = d, add = d + 1, subtract = d - 1)
#' data.frame(date = d, add = d + "1 year", subtract = d - "1 year")
#' as_messydate("2001-01-01") + as_messydate("2001-01-02..2001-01-04")
#' as_messydate("2001-01-01") + as_messydate("2001-01-03")
#' as_messydate("2001-01-01..2001-01-04") - as_messydate("2001-01-02")
#' #as_messydate("2001-01-01") - as_messydate("2001-01-03")
#' # calendar (month/year) arithmetic keeps the day of month and time of day
#' as_messydate("2012-01-31 09:00") + "1 month"
#' # sub-day units shift the instant
#' as_messydate("2012-01-01 14:30:00") + "2 hours"
#' }
#' @name operate_arithmetic
NULL

#' @rdname operate_arithmetic
#' @export
`+.mdate` <- function(e1, e2) {
  if (is_time_arithmetic(e1, e2)) return(shift_time(e1, e2, 1))
  e2 <- parse_date_strings(e2)
  add(e1, e2)
}

#' @rdname operate_arithmetic
#' @export
`-.mdate` <- function(e1, e2) {
  if (is_time_arithmetic(e1, e2)) return(shift_time(e1, e2, -1))
  e2 <- parse_date_strings(e2)
  subtract(e1, e2)
}

# Time arithmetic applies when the operand carries a time of day, or when the
# amount is expressed in sub-day units (hours, minutes, seconds).
is_time_arithmetic <- function(e1, e2) {
  if (is_messydate(e2)) return(FALSE)
  has_time <- any(grepl("[T ]", as.character(e1)))
  sub_day <- is.character(e2) && any(grepl("hour|min|sec", e2))
  cal_day <- is.character(e2) && any(grepl("year|month|week|day", e2))
  sub_day || (has_time && (is.numeric(e2) || cal_day))
}

# Shifts date-times by an amount. Year and month amounts shift the calendar
# components (preserving the day and time of day); all other units shift the
# instant in seconds. Date-only operands are promoted to a time when the shift
# is sub-day.
shift_time <- function(e1, e2, sign) {
  shifter <- make_shifter(e2, sign)
  out <- vapply(as.character(e1), function(y) {
    # An open range's leading/trailing ".." must be checked (and restored)
    # explicitly: base::strsplit() silently drops a trailing empty field, so
    # splitting "2012-01-01 09:00.." on ".." would otherwise lose the
    # "on or after" marker entirely.
    if (grepl("^\\.\\.", y)) return(paste0("..", shifter(sub("^\\.\\.", "", y))))
    if (grepl("\\.\\.$", y)) return(paste0(shifter(sub("\\.\\.$", "", y)), ".."))
    if (grepl("\\.\\.", y)) {
      ends <- strsplit(y, "\\.\\.")[[1]]
      return(paste(vapply(ends, shifter, character(1)), collapse = ".."))
    }
    shifter(y)
  }, character(1), USE.NAMES = FALSE)
  as_messydate(out)
}

# Returns a function that shifts one date-time string. Calendar units (year,
# month) add a period to the date part; everything else adds seconds.
make_shifter <- function(e2, sign) {
  unit <- if (is.character(e2)) {
    if (grepl("year", e2)) "year" else if (grepl("month", e2)) "month" else NA
  } else NA
  if (!is.na(unit)) {
    n <- sign * as.numeric(stringi::stri_replace_all_regex(e2, "[^0-9.-]", ""))
    period <- lubridate::period(n, units = unit)
    function(y) shift_calendar(y, period)
  } else {
    secs <- sign * parse_seconds(e2)
    function(y) shift_one(y, secs)
  }
}

# Adds a calendar period to the date part of a date-time, keeping the time of
# day and any UTC offset unchanged.
shift_calendar <- function(y, period) {
  if (!nzchar(y)) return(y)
  off <- regmatches(y, regexpr("(Z|[+-][0-9]{2}:[0-9]{2})$", y))
  base <- sub("(Z|[+-][0-9]{2}:[0-9]{2})$", "", y)
  datepart <- sub("[T ].*$", "", base)
  timepart <- if (grepl("[T ]", base)) sub("^[^T ]*[T ]", "", base) else ""
  d <- lubridate::add_with_rollback(as.Date(datepart), period)
  paste0(format(d), if (nzchar(timepart)) paste0(.dt_sep, timepart) else "",
         if (length(off)) off else "")
}

shift_one <- function(y, secs) {
  if (!nzchar(y)) return(y)
  off <- regmatches(y, regexpr("(Z|[+-][0-9]{2}:[0-9]{2})$", y))
  base <- sub("(Z|[+-][0-9]{2}:[0-9]{2})$", "", y)
  p <- as.POSIXct(sub("T", " ", base, fixed = TRUE), tz = "UTC") + secs
  paste0(format(p, paste0("%Y-%m-%d", .dt_sep, "%H:%M:%S")),
         if (length(off)) off else "")
}

# Converts a shift amount to seconds. Numeric amounts are days.
parse_seconds <- function(e2) {
  if (is.numeric(e2)) return(e2 * 86400)
  num <- as.numeric(stringi::stri_replace_all_regex(e2, "[^0-9.-]", ""))
  unit <- c(year = 365 * 86400, month = 30.42 * 86400, week = 7 * 86400,
            day = 86400, hour = 3600, min = 60, sec = 1)
  key <- names(unit)[vapply(names(unit), function(u) grepl(u, e2), logical(1))][1]
  num * (if (is.na(key)) 86400 else unit[[key]])
}

add <- function(x, n) {
  if (is_messydate(n)) {
    x <- suppressMessages(expand(x)[[1]])
    n <- suppressMessages(expand(n)[[1]])
    if (any(is.element(n, x))) {
      n <- n[which(!is.element(n, x))]
    }
    x <- suppressMessages(contract(paste(c(x, n), collapse = ", ")))
  } else {
    # Step one: get only first and last components for ranges
    # But keep approximation for before or after date
    x <- ifelse(stringi::stri_detect_regex(x, "^\\.\\.|\\.\\.$"), x, expand(x))
    # Step two, add by component
    x <- lapply(x, function(y) {
      if (stringi::stri_detect_regex(y[1], "^-")) {
        y <- paste0("-", lubridate::as_date(y) - n)
      } else if (stringi::stri_detect_regex(y[1], "^\\.\\.")) {
        y <- stringi::stri_replace_all_regex(y, "\\.\\.", "")
        y <- paste0("..", lubridate::as_date(y) + n)
      } else if (stringi::stri_detect_regex(y[1], "\\.\\.$")) {
        y <- stringi::stri_replace_all_regex(y, "\\.\\.", "")
        y <- paste0(lubridate::as_date(y) + n, "..")
      } else {
        y <- lubridate::as_date(y) + n
      }
      y
    })
    x <- suppressMessages(contract(x))
  }
  x
}

subtract <- function(x, n) {
  if (is_messydate(n)) {
    x <- as.character(expand(x)[[1]])
    n <- as.character(expand(n)[[1]])
    if (any(is.element(x, n))) {
      x <- as_messydate(list(x[which(!is.element(x,n))]))
    } else {
      message("First and second elements do not overlap.")
      x <- as_messydate(c(x, n))
    }
  } else {
    # Step one: get only first and last components for ranges
    # But keep approximation for before or after date
    x <- ifelse(stringi::stri_detect_regex(x, "^\\.\\.|\\.\\.$"), x, expand(x))
    # Step two, add by component
    x <- lapply(x, function(y) {
      if (stringi::stri_detect_regex(y[1], "^-")) {
        y <- paste0("-", lubridate::as_date(y) + n)
      } else if (stringi::stri_detect_regex(y[1], "^\\.\\.")) {
        y <- stringi::stri_replace_all_regex(y, "\\.\\.", "")
        y <- paste0("..", lubridate::as_date(y) - n)
      } else if (stringi::stri_detect_regex(y[1], "\\.\\.$")) {
        y <- stringi::stri_replace_all_regex(y, "\\.\\.", "")
        y <- paste0(lubridate::as_date(y) - n, "..")
      } else {
        y <- lubridate::as_date(y) - n
      }
      y
    })
    x <- suppressMessages(contract(x))
  }
  x
}

parse_date_strings <- function(e2) {
  if (is_messydate(e2)) {
    e2 <- contract(e2)
  } else {
    e2 <- ifelse(stringi::stri_detect_regex(e2, "years|year"),
                 as.numeric(stringi::stri_replace_all_regex(e2, "years|year", "")) * 365, e2)
    e2 <- ifelse(stringi::stri_detect_regex(e2, "months|month"),
                 as.numeric(stringi::stri_replace_all_regex(e2, "months|month", "")) * 30.42, e2)
    e2 <- ifelse(stringi::stri_detect_regex(e2, "days|day"),
                 as.numeric(stringi::stri_replace_all_regex(e2, "days|day", "")), e2)
  }
  e2
}
