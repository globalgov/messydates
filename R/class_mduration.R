#' A flexible duration class for messy durations
#' @description
#'   Most R packages handle duration and periods as exact time or date intervals.
#'   However, this is not possible for 'messy' dates where uncertainty or
#'   approximation might be present.
#'   The `mduration` class accounts for uncertainty and approximation
#'   in `mdate` objects to return their duration as a range of possible dates.
#'
#'   Non-range values (a single date, or a range collapsed to a single value)
#'   are returned unchanged.
#'   When both ends of the range carry a time of day, `approx_range` is
#'   still interpreted as a number of days, but the returned range keeps
#'   sub-day precision (e.g. `"2010-01-01 09:00..2010-01-01 17:00"`).
#' @param x An `mdate` variable with ranges.
#' @param approx_range Range to expand approximate dates, in days.
#'   If 3, for example, widens the range by 3 days on both sides,
#'   moving the start 3 days earlier and the end 3 days later;
#'   if -3, narrows the range by 3 days from both sides.
#' @return Object of class `mduration`
#' @name class_mduration
#' @examples
#' messyduration(as_messydate(c("2010-01-01..2010-12-31", "2010-01..2010-12")))
#' # widen (or narrow) the range at both ends
#' messyduration(as_messydate("2010-06-01..2010-06-10"), approx_range = 3)
#' # ranges that carry a time of day keep sub-day precision
#' messyduration(as_messydate("2010-01-01 09:00..2010-01-01 17:00"))
NULL

#' @rdname class_mduration
#' @export
new_messyduration <- function(x = character()) {
  stopifnot(is.character(x))
  structure(x, class = "mduration")
}

#' @rdname class_mduration
#' @export
validate_messyduration <- function(x, approx_range = 0) {
  if (any(!grepl("\\.\\.", x))) {
    stop("mduration class objects should have at least one date range",
         call. = FALSE)
  }
}

#' @rdname class_mduration
#' @export
make_messyduration <- function(x, approx_range = 0) UseMethod("make_messyduration")

#' @rdname class_mduration
#' @export
make_messyduration.character <- function(x, approx_range = 0) {
  message("Converting to mdate class.")
  x <- as_messydate(x)
  validate_messyduration(x)
  x <- ifelse(grepl("\\.\\.", x), messy_range(x, approx_range), x)
  new_messyduration(x)
}

#' @rdname class_mduration
#' @export
make_messyduration.mdate <- function(x, approx_range = 0) {
  validate_messyduration(x)
  x <- ifelse(grepl("\\.\\.", x), messy_range(x, approx_range), x)
  new_messyduration(x)
}

messy_range <- function(x, approx_range) {
  parts <- strsplit(x, "\\.\\.")
  starts <- vapply(parts, `[`, character(1), 1)
  ends <- vapply(parts, `[`, character(1), 2)
  if (any(grepl("[T ]", c(starts, ends)))) {
    # Date-time ranges keep sub-day precision via POSIXct.
    s <- mdate_to_posixct(starts) - approx_range * 86400
    e <- mdate_to_posixct(ends) + approx_range * 86400
    as_messydate(paste0(format(s, paste0("%Y-%m-%d", .dt_sep, "%H:%M:%S")), "..",
                        format(e, paste0("%Y-%m-%d", .dt_sep, "%H:%M:%S"))))
  } else {
    dates1 <- as.Date(as_messydate(starts), FUN = min) - approx_range
    dates2 <- as.Date(as_messydate(ends), FUN = max) + approx_range
    as_messydate(paste0(dates1, "..", dates2))
  }
}

#' @rdname class_mduration
#' @importFrom utils str
#' @export
print.mduration <- function(x, ...) {
  str(x)
}
