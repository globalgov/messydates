#' Sequence method for messydates
#' @description
#'   This function provides a sequence (`seq()`) method for messydates.
#'   This can be used with ranges or unspecified dates,
#'   and is particularly useful for defining a sequence of dates
#'   before the common era or between eras.
#' @details
#'   If `from`/`to` (or `by`) carry a time of day, the sequence is
#'   generated at the requested sub-day granularity (e.g. `by = "hour"`)
#'   via `POSIXct`, and each element of the result keeps a time of day.
#'   Otherwise, dates are sequenced by calendar day (or another day-based
#'   `by`, e.g. `"week"` or `"month"`). Because years are numbered
#'   astronomically (proleptic Gregorian, with a year zero), a sequence that
#'   spans the BCE/CE boundary passes through the astronomical year zero
#'   (= 1 BCE) rather than jumping straight from `-0001` to `0001`.
#' @name convert_sequence
#' @param from A messydate or range.
#'   If 'from' is a range and 'to' is not specified,
#'   'from' will be the minimum of the range and 'to' will be maximum.
#' @param to A messydate.
#' @param by Increment of the sequence. By default "days". Use a sub-day
#'   unit ("hour", "min", or "sec") for a date-time sequence.
#' @param ... Arguments passed to or from methods.
#' @examples
#' seq(mdate("-0001-12-20"), mdate("0001-01-10"))
#' # a range's endpoints are used when only 'from' is given
#' seq(as_messydate("2012-01-01..2012-01-05"))
#' # date-time sequences use a sub-day 'by'
#' seq(as_messydate("2019-03-01 09:00"), as_messydate("2019-03-01 12:00"),
#'     by = "hour")
#' @export
seq.mdate <- function(from, to, by = "days", ...) {

  if(missing(to) & !is_precise(from)){
    to <- max(from)
    from <- min(from)
  }

  # sub-day steps, or endpoints carrying a time of day, use POSIXct
  if (grepl("hour|min|sec", by) ||
      any(grepl("[T ]", c(as.character(from), as.character(to))))) {
    s <- as.POSIXct(sub("T", " ", fixed = TRUE,
                        sub("(Z|[+-][0-9]{2}:[0-9]{2})$", "",
                            as.character(from))), tz = "UTC")
    e <- as.POSIXct(sub("T", " ", fixed = TRUE,
                        sub("(Z|[+-][0-9]{2}:[0-9]{2})$", "",
                            as.character(to))), tz = "UTC")
    return(format(seq(s, e, by = by), paste0("%Y-%m-%d", .dt_sep, "%H:%M:%S")))
  }

  # straight forward sequence (all common-era dates): return Date objects
  if(!any(is_bce(c(from, to)))){
    seq(as.Date(from), as.Date(to), by = by)
  } else {
    # Any endpoint before the common era. `as.Date.mdate()` maps mdate strings
    # to R's `Date`, which uses the proleptic Gregorian calendar with
    # astronomical year numbering (a year zero exists), so a single `seq.Date()`
    # walks the whole continuum uniformly -- through the 366-day year zero and
    # across the BCE/CE boundary -- with no era special-casing. `zero_padding()`
    # re-pads the years that R formats as e.g. "-001" back to "-0001".
    zero_padding(format(seq(as.Date(from), as.Date(to), by = by), "%Y-%m-%d"))
  }
}
