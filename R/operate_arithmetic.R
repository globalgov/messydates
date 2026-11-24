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
#'    invalid `"2012-02-31"`. Each run of days moves by its first and last
#'    day, and a run that covers whole months stays whole months, so
#'    `"2012-02"` plus one month gives `"2012-03"`.}
#'    \item{Sub-day units, or any unit at all when `e1` carries a time of
#'    day, shift the instant in seconds via `POSIXct`, promoting a
#'    date-only `e1` to a time if the shift is sub-day.}
#'   }
#'   When `e2` is itself an `mdate`, `+`/`-` instead treat both sides as
#'   sets of dates: `+` returns their union (as a multiset, in the most
#'   succinct `mdate` notation; see also `?operate_set` for `%union%`, which
#'   works element by element instead), and `-` removes
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
  # Shifting the empty set gives the empty set, and shifting NA gives NA.
  if (.has_missing(e1)) return(.with_missing(e1, `+.mdate`, e2))
  if (is_time_arithmetic(e1, e2)) return(shift_time(e1, e2, 1))
  e2 <- parse_date_strings(e2)
  add(e1, e2)
}

#' @rdname operate_arithmetic
#' @export
`-.mdate` <- function(e1, e2) {
  if (!missing(e2) && .has_missing(e1))
    return(.with_missing(e1, `-.mdate`, e2))
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
    x <- suppressMessages(possible_dates(x)[[1]])
    n <- suppressMessages(possible_dates(n)[[1]])
    if (any(is.element(n, x))) {
      n <- n[which(!is.element(n, x))]
    }
    x <- suppressMessages(.contract(paste(c(x, n), collapse = ", ")))
  } else {
    # Step one: get only first and last components for ranges
    # But keep approximation for before or after date
    x <- ifelse(stringi::stri_detect_regex(x, "^\\.\\.|\\.\\.$"), x, possible_dates(x))
    # Step two, add by component
    x <- lapply(x, .shift_value, n = n, sign = 1)
    x <- suppressMessages(.contract(x))
  }
  x
}

subtract <- function(x, n) {
  if (is_messydate(n)) {
    x <- as.character(possible_dates(x)[[1]])
    n <- as.character(possible_dates(n)[[1]])
    if (any(is.element(x, n))) {
      x <- as_messydate(list(x[which(!is.element(x,n))]))
    } else {
      message("First and second elements do not overlap.")
      x <- as_messydate(c(x, n))
    }
  } else {
    # Step one: get only first and last components for ranges
    # But keep approximation for before or after date
    x <- ifelse(stringi::stri_detect_regex(x, "^\\.\\.|\\.\\.$"), x, possible_dates(x))
    # Step two, add by component
    x <- lapply(x, .shift_value, n = n, sign = -1)
    x <- suppressMessages(.contract(x))
  }
  x
}

parse_date_strings <- function(e2) {
  if (is_messydate(e2)) return(.contract(e2))
  if (!is.character(e2)) return(e2)
  n <- as.numeric(stringi::stri_replace_all_regex(e2, "[^0-9.-]", ""))
  if (stringi::stri_detect_regex(e2, "year")) return(lubridate::period(n, units = "year"))
  if (stringi::stri_detect_regex(e2, "month")) return(lubridate::period(n, units = "month"))
  if (stringi::stri_detect_regex(e2, "week")) return(n * 7)
  if (stringi::stri_detect_regex(e2, "day")) return(n)
  e2
}

# Shifts one value, given as its possible days or as an open range, by `n`.
# The days are read as true day numbers, so that a date before the common
# era moves forward in time when a positive amount is added:
# "-0050-01-01" plus one day is "-0050-01-02".
.shift_value <- function(y, n, sign) {
  before <- stringi::stri_detect_regex(y[1], "^\\.\\.")
  after <- stringi::stri_detect_regex(y[1], "\\.\\.$")
  y <- stringi::stri_replace_all_regex(y, "\\.\\.", "")
  out <- .from_days(.shift_days(.to_days(y), n, sign))
  if (before) out <- paste0("..", out)
  if (after) out <- paste0(out, "..")
  out
}

# Reads ISO day strings, including dates before the common era, as Dates.
.to_days <- function(y) as.Date(new_messydate(y), FUN = identity)

# Writes Dates as ISO day strings, with four digits in each year.
.from_days <- function(d) zero_padding(format(d, "%Y-%m-%d"))

# Shifts days by a number of days, or by a calendar period of months or
# years. For a period, each run of consecutive days is shifted by its first
# and last day. A day keeps its day of the month, and rolls back to the last
# valid day where that day does not exist: 2012-02-29 plus one year gives
# 2013-02-28. A run that covers whole months stays whole months, so that
# "2012-02" plus one month gives all of March.
.shift_days <- function(d, n, sign) {
  if (!inherits(n, "Period")) return(d + sign * n)
  d <- sort(unique(d))
  if (length(d) == 0) return(d)
  p <- sign * n
  runs <- split(d, cumsum(c(1, diff(as.numeric(d)) != 1)))
  out <- lapply(runs, function(r) {
    lo <- r[1]
    hi <- r[length(r)]
    new_lo <- lubridate::add_with_rollback(lo, p)
    new_hi <- lubridate::add_with_rollback(hi, p)
    whole <- length(r) > 1 && lubridate::mday(lo) == 1 &&
      lubridate::mday(hi) == lubridate::days_in_month(hi)
    if (whole)
      new_hi <- new_hi - lubridate::mday(new_hi) + lubridate::days_in_month(new_hi)
    seq(new_lo, new_hi, by = "day")
  })
  sort(unique(do.call(c, unname(out))))
}
