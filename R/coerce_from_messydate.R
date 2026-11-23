#' Coercion from `mdate` to common date classes
#' @description
#'   These functions coerce objects of `mdate` class to
#'   common date classes such as `Date`, `POSIXct`, and `POSIXlt`.
#'   Since `mdate` objects can hold multiple individual dates, however,
#'   an additional function must be passed as an argument so that
#'   these functions know how to resolve multiple dates into a single date.
#'
#'   For example, one might wish to use the earliest possible date
#'   in any ranges of dates (`min`), the latest possible date (`max`),
#'   some notion of a central tendency (`mean`, `median`, or `modal`),
#'   or even a `random` selection from among the candidate dates.
#'
#'   These functions then, building on `possible_dates()` and the resolve functions,
#'   are particularly useful in converting back out of the `mdate` class
#'   for use with existing methods and models,
#'   especially for checking the robustness of results.
#' @param x A `mdate` object
#' @param ... Arguments passed on to the S3 generics.
#' @param FUN A function that can be used to resolve expanded messy dates
#'   into a single date.
#'   For example, `min()`, `max()`, `mean()`, `median()`,
#'   `modal()`, and `random()`. `vmin()`, `vmax()`, `vmean()`, `vmedian()`,
#'   `vmodal()`, and `vrandom()` are the vectorised equivalents, resolving
#'   each element separately rather than summarising the whole vector.
#'
#'   A precise value needs no `FUN`.
#'   If `x` holds an imprecise value and you do not give `FUN`,
#'   the function uses `vmin()`, the earliest possible date,
#'   and states this rule in a message once in each session.
#'   In messydates 1.3.0, the message becomes a warning.
#'   In messydates 1.4.0, it becomes an error:
#'   then you must give `FUN` to convert an imprecise value.
#' @return A date object of `Date`, `POSIXct`, or `POSIXlt` class
#' @family coerce
#' @seealso [resolve_extrema()], [resolve_tendency()], [resolve_probabilistically()]
#' @name coerce_from
NULL

#' @rdname coerce_from
#' @details
#'   `as.Date()` always drops any time of day carried by `x` (a calendar
#'   date has no time component); use `as.POSIXct()` or `as.POSIXlt()` to
#'   keep the time.
#' @examples
#' as.Date(as_messydate("2012-01"), FUN = vmin)
#' as.Date(as_messydate("2012-01-01"), FUN = vmean)
#' as.Date(as_messydate("2012-01"), FUN = vmax)
#' as.Date(as_messydate("2012-01"), FUN = vmedian)
#' as.Date(as_messydate("2012-01"), FUN = vmodal)
#' as.Date(as_messydate("2012-01"), FUN = vrandom)
#' # "1000 BC" is the astronomical year -0999 (year zero exists), whereas the
#' # signed ISO "-1000" below is astronomical year -1000
#' as.Date(as_messydate("1000 BC"), FUN = vmax)
#' as.Date(as_messydate("1000 BC"), FUN = vmedian)
#' as.Date(as_messydate(c("-1000", "2020")), FUN = vmin)
#' # the time of day, if any, is dropped
#' as.Date(as_messydate("2012-01-01 14:30"), FUN = vmin)
#' @export
as.Date.mdate <- function(x, FUN, ...) {
  x <- .resolve_for(x, FUN, missing(FUN), "as.Date", ...)
  # A calendar date has no time of day, so drop any time components.
  x <- strip_times(as.character(x))
  x <- suppressWarnings(ifelse(stringi::stri_detect_regex(x, "^-"),
                               lubridate::as_date(negative_dates(x)),
                               lubridate::as_date(zero_padding(x))))
  as.Date(x, origin = "1970-01-01")
}

#' @rdname coerce_from
#' @param tz Character string specifying the time zone for the conversion,
#'   if required.
#'   By default "UTC" (Universal Time Coordinated), equivalent to GMT.
#'   If "" then the current time zone is used.
#' @details
#'   `as.POSIXct()` and `as.POSIXlt()` keep the time of day (defaulting to
#'   midnight if `x` is date-only), and honour a UTC offset if `x` carries
#'   one. They do not support dates before the common era; use `as.Date()`
#'   for those.
#' @examples
#' as.POSIXct(as_messydate("2012-01-01 14:30:00"), FUN = vmin)
#' as.POSIXct(as_messydate("2012-01-01 14:30:00+02:00"), FUN = vmin)
#' as.POSIXlt(as_messydate("2012-01-01 14:30:00"), FUN = vmin)
#' @export
as.POSIXct.mdate <- function(x, tz = "UTC", FUN, ...) {
  x <- .resolve_for(x, FUN, missing(FUN), "as.POSIXct", ...)
  # Vectorised guard: any() rather than a bare if(), which errors on a
  # length > 1 condition (R >= 4.2) and so broke conversion of vectors.
  if (any(stringi::stri_detect_regex(as.character(x), "^-"), na.rm = TRUE)) {
    stop("For conversion of negative dates from mdate class use as.Date()")
  }
  mdate_to_posixct(as.character(x), tz = tz)
}

#' @rdname coerce_from
#' @export
as.POSIXlt.mdate <- function(x, tz = "UTC", FUN, ...) {
  x <- .resolve_for(x, FUN, missing(FUN), "as.POSIXlt", ...)
  # Vectorised guard: any() rather than a bare if(), which errors on a
  # length > 1 condition (R >= 4.2) and so broke conversion of vectors.
  if (any(stringi::stri_detect_regex(as.character(x), "^-"), na.rm = TRUE)) {
    stop("For conversion of negative dates from mdate class use as.Date()")
  }
  as.POSIXlt(mdate_to_posixct(as.character(x), tz = tz))
}

# Parses a canonical mdate string (date or date-time, with optional 'Z'/offset)
# into POSIXct. When an offset is present the instant is honoured; otherwise the
# clock time is interpreted in `tz`. Accepts either the canonical space
# separator or (for robustness) a 'T', normalising to a space throughout so
# a single fixed format string can be used below.
mdate_to_posixct <- function(s, tz = "UTC") {
  s <- sub("T", " ", s, fixed = TRUE)
  has_off <- grepl("(Z|[+-][0-9]{2}:[0-9]{2})$", s)
  out <- as.POSIXct(rep(NA_real_, length(s)), tz = tz)
  if (any(!has_off)) {
    out[!has_off] <- as.POSIXct(s[!has_off], tz = tz)
  }
  if (any(has_off)) {
    z <- sub("Z$", "+0000", s[has_off])
    z <- gsub("([+-][0-9]{2}):([0-9]{2})$", "\\1\\2", z)
    # Ensure offset-bearing strings include seconds for the fixed format below.
    z <- gsub("( [0-9]{2})([+-][0-9]{4})$", "\\1:00:00\\2", z)
    z <- gsub("( [0-9]{2}:[0-9]{2})([+-][0-9]{4})$", "\\1:00\\2", z)
    out[has_off] <- as.POSIXct(z, format = "%Y-%m-%d %H:%M:%OS%z", tz = tz)
  }
  out
}

# Helper function for returning negative dates in date formats
#' @importFrom stringi stri_replace_all_regex stri_extract_all_regex
#' @importFrom lubridate ymd years as_date
negative_dates <- function(x) {
  x <- stringi::stri_replace_all_regex(x, "^-", "")
  y <- stringi::stri_extract_all_regex(x, "^[0-9]{4}")
  md <- stringi::stri_replace_all_regex(x, "^[0-9]{4}", "0000")
  x <- lubridate::ymd(md) - lubridate::years(y)
  x <- lubridate::as_date(x)
  x
}

#' @rdname coerce_from
#' @details
#'   `as.data.frame()` places the (unresolved) `mdate` vector in a
#'   single-column data frame, as for any other vector.
#' @examples
#' as.data.frame(as_messydate(c("2012-01-01", "2012-02")))
#' @export
as.data.frame.mdate <- function(x, ...) {
  as.data.frame.vector(x, ...)
}

#' @rdname coerce_from
#' @details
#'   `as.double()` converts `x` to the number of days since 1970-01-01,
#'   as `as.double(as.Date(x, FUN))` does.
#'   An imprecise value is first resolved with `FUN`, as for `as.Date()`.
#' @examples
#' as.double(as_messydate("2012-01-01"))
#' as.double(as_messydate("2012-01"), FUN = vmax)
#' @export
as.double.mdate <- function(x, FUN, ...) {
  x <- .resolve_for(x, FUN, missing(FUN), "as.double", ...)
  as.double(as.Date(x, FUN = identity))
}

# Resolves `x` with `FUN` before a conversion to an exact class.
# Further arguments (`...`) are passed to `FUN`.
# Without `FUN`, a precise value needs no rule. If an imprecise value is
# present, the function uses vmin() and states the rule once in each session.
# In 1.3.0, this message becomes a warning each time, and in 1.4.0 an error.
.resolve_for <- function(x, FUN, fun_missing, caller, ...) {
  # Further arguments, such as approx_range, go to the rule.
  if (!fun_missing) return(FUN(x, ...))
  n <- sum(.needs_resolution(x))
  if (n > 0) .declare_resolution(caller, n)
  vmin(x)
}

# Which elements of `x` are imprecise and so need a rule to resolve them?
# A missing value and the empty set resolve to NA with any rule.
.needs_resolution <- function(x) {
  out <- !date_is_precise(x) & !is_empty_notation(x)
  out[is.na(out) | is.na(as.character(x))] <- FALSE
  out
}

.declare_resolution <- function(caller, n) {
  key <- paste0("resolve:", caller)
  if (isTRUE(.deprecated_env[[key]])) return(invisible())
  .deprecated_env[[key]] <- TRUE
  message("`", caller, "()` resolved ", n, " imprecise value",
          if (n > 1) "s", " with `vmin()`, the earliest possible date.\n",
          "Give `FUN` to choose the rule, ",
          "for example `", caller, "(x, FUN = vmax)`.\n",
          "In messydates 1.3.0, this message becomes a warning, ",
          "and in 1.4.0, an error.\n",
          "This message is shown once in each session.")
  invisible()
}

# {lubridate}'s as_date() is an S4 generic whose fallback calls base::as.Date(),
# which dispatches to as.Date.mdate() above, so as_date(<mdate>) already works
# (honouring `FUN`). as_datetime(), however, derives a default time zone via
# lubridate::tz(x); for an mdate that now returns the ISO *offset* designator
# (or NA), not an Olson zone name, which as.POSIXct() would reject. Register an
# S4 method that goes straight to as.POSIXct.mdate() instead, so
# as_datetime(<mdate>) behaves exactly like as.POSIXct(<mdate>).
#' @importFrom methods setOldClass setMethod
methods::setOldClass("mdate")

#' @rdname coerce_from
#' @details
#'   `{lubridate}`'s `as_date()` and `as_datetime()` also accept an `mdate`
#'   (delegating to `as.Date()`/`as.POSIXct()` above, so the `FUN` resolver
#'   still applies).
#' @aliases as_datetime,mdate-method
#' @importFrom lubridate as_datetime
#' @exportMethod as_datetime
methods::setMethod("as_datetime", "mdate", function(x, ...) {
  as.POSIXct(x, ...)
})

