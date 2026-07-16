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
#'   These functions then, building on `expand()` and the resolve functions,
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
#' @return A date object of `Date`, `POSIXct`, or `POSIXlt` class
#' @family coerce
#' @seealso [resolve_extrema()], [resolve_tendency()]
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
#' as.Date(as_messydate("1000 BC"), FUN = vmax)
#' as.Date(as_messydate("1000 BC"), FUN = vmedian)
#' as.Date(as_messydate(c("-1000", "2020")), FUN = vmin)
#' # the time of day, if any, is dropped
#' as.Date(as_messydate("2012-01-01 14:30"), FUN = vmin)
#' @export
as.Date.mdate <- function(x, FUN = vmin, ...) {
  # # fix argument ordering issues
  # if (missing(FUN)){
  #   if(length(list(...)) > 0) FUN <- list(...)[[1]] else
  #     FUN <- messydates::min.mdate
  # }
  # if(missing(FUN)) FUN <- min
  x <- FUN(x)
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
as.POSIXct.mdate <- function(x, tz = "UTC", FUN = vmin, ...) {
  # if (missing(FUN) & length(list(...)) > 0) FUN <- list(...)[[1]]
  x <- FUN(x)
  # Vectorised guard: any() rather than a bare if(), which errors on a
  # length > 1 condition (R >= 4.2) and so broke conversion of vectors.
  if (any(stringi::stri_detect_regex(as.character(x), "^-"), na.rm = TRUE)) {
    stop("For conversion of negative dates from mdate class use as.Date()")
  }
  mdate_to_posixct(as.character(x), tz = tz)
}

#' @rdname coerce_from
#' @export
as.POSIXlt.mdate <- function(x, tz = "UTC", FUN = vmin, ...) {
  # if (missing(FUN) & length(list(...)) > 0) FUN <- list(...)[[1]]
  x <- FUN(x)
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
#'   `as.list()` splits `x` into a list of length-one `mdate` objects,
#'   one per element, without resolving any of them.
#' @examples
#' as.list(as_messydate(c("2012-01-01", "2012-02")))
#' @export
as.list.mdate <- function(x, ...) {
  lapply(unclass(x), as_messydate)
}

#' @rdname coerce_from
#' @details
#'   `as.double()` converts `x` to the number of days since 1970-01-01 (as
#'   for `as.double(as.Date(x))`), without resolving ranges, sets, or
#'   unspecified components first; it is mostly useful for already-precise
#'   dates.
#' @examples
#' as.double(as_messydate("2012-01-01"))
#' @export
as.double.mdate <- function(x, ...) {
  if(any(is_bce(x))) x[is_bce(x)] <- negative_dates(x)[is_bce(x)]
  as.double(lubridate::as_date(x))
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

