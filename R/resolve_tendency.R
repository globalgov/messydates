#' Resolves messy dates into a central tendency
#' @description
#'   These functions resolve messydates by their central tendency.
#'   While the functions `mean()`, `median()`, and `modal()` expand *all*
#'   elements of the vector into one combined set of dates and summarise it
#'   to a single value (matching the usual behaviour of these generics),
#'   the `v*()` versions resolve each element separately and so return a
#'   vector of the same length as the input.
#' @details
#'   All of these functions work by calling `possible_dates()` to enumerate the
#'   dates or date-times consistent with each messy value, then summarising
#'   that expanded set. For `median()` and `mean()`, an even number of
#'   expanded values is resolved by averaging the two middle values (via
#'   `POSIXct` when a time of day is present, or `Date` otherwise); an odd
#'   number simply returns the middle value.
#'
#'   Both kinds of set are expanded to their members and summarised the same
#'   way, but the result means different things for each. For a `{}` ("all
#'   members of") set, which records that something happened on several
#'   dates, the central tendency describes where those occurrences sit. For a
#'   `[]` ("one member of") set, which records that something happened on
#'   exactly one of those dates but not which, the central tendency is a point
#'   estimate of that single unknown date, as it is for a range or an
#'   unspecified component.
#'
#'   Averaging across the BCE/CE boundary, or between two BCE dates, is not
#'   currently supported: `median()` falls back to the earlier of the two
#'   middle values in that case, and `mean()`/`vmean()` may be unreliable for
#'   solely negative-year inputs (a documented limitation, not a supported
#'   feature).
#' @name resolve_tendency
#' @inheritParams resolve_extrema
#' @examples
#' d <- as_messydate(c("2008-03-25", "?2012-02-27", "2001-01?", "2001~",
#'   "2001-01-01..2001-02-02", "{2001-01-01,2001-02-02}",
#'   "{2001-01,2001-02-02}", "2008-XX-31", "-0050-01-01"))
#' d
#' # the time of day is honoured when averaging precise date-times
#' r <- as_messydate(c("2012-06-01 09:00", "2012-06-01 17:00"))
#' median(r)
#' mean(r)
NULL

#' @rdname resolve_tendency
#' @importFrom stats median
#' @examples
#' median(d)
#' @export
median.mdate <- function(..., na.rm = TRUE, uncertain = "keep"){

  x <- list(...)[[1]]

  x <- .drop_empty(x)

  x <- .handle_uncertain(x, uncertain, elementwise = FALSE)
  y <- unlist(possible_dates(x))
  y <- .order_messy(y)
  .median_value(y, na.rm = na.rm)
}

.order_messy <- function(y){
  if(any(date_is_bce(y))){
    bcey <- y[date_is_bce(y)]
    cey <- y[!date_is_bce(y)]
    c(bcey[order(bcey, decreasing = TRUE)],
           cey[order(cey)])
  } else {
    y[order(y)]
  }
}

# Computes the median of an already-ordered vector of canonical date or
# date-time strings. For an odd-length vector this is simply the middle
# value; for an even-length vector, the two middle values are averaged (via
# POSIXct when a time of day is present, or Date otherwise). Base R's
# `median()` cannot average two character strings, which is why this helper
# exists rather than calling `median()` directly on `y`.
# Averaging across the BCE/CE boundary is not supported: the earlier of the
# two middle values is returned instead.
.median_value <- function(y, na.rm = TRUE) {
  if (na.rm) y <- y[!is.na(y)]
  n <- length(y)
  if (n == 0) return(NA_character_)
  if (n %% 2 == 1) return(y[(n + 1) / 2])
  lo <- y[n / 2]
  hi <- y[n / 2 + 1]
  if (any(date_is_bce(c(lo, hi)))) return(lo)
  if (any(grepl("[T ]", c(lo, hi))))
    return(format(mean(mdate_to_posixct(c(lo, hi))),
                  paste0("%Y-%m-%d", .dt_sep, "%H:%M:%S")))
  as.character(mean(as.Date(c(lo, hi))))
}

#' @rdname resolve_tendency
#' @export
vmedian <- function(..., na.rm = TRUE) UseMethod("vmedian")

#' @rdname resolve_tendency
#' @importFrom stats median
#' @examples
#' vmedian(d)
#' @export
vmedian.mdate <- function(..., na.rm = TRUE, uncertain = "keep"){

  x <- list(...)[[1]]

  x <- .empty_to_na(x)

  x <- .handle_uncertain(x, uncertain, elementwise = TRUE)
  f <- .full_dates(x)
  out <- f$out
  out[f$rng] <- format(f$lo + (f$n - 1L) %/% 2L, "%Y-%m-%d")
  out[f$rest] <- vapply(as.list(x[f$rest]), function(y) {
    if (is.na(y)) return(NA_character_)
    .median_value(.order_messy(unlist(possible_dates(y))), na.rm = na.rm)
  }, FUN.VALUE = character(1))
  out
}

#' @rdname resolve_tendency
#' @param trim the fraction (0 to 0.5) of observations to be trimmed
#'   from each end of x before the mean is computed.
#'   Values of trim outside that range are taken as the nearest endpoint.
#' @importFrom lubridate as_date
#' @examples
#' mean(d)
#' @export
mean.mdate <- function(..., trim = 0, na.rm = TRUE, uncertain = "keep"){

  x <- list(...)[[1]]

  x <- .drop_empty(x)

  x <- .handle_uncertain(x, uncertain, elementwise = FALSE)
  y <- unlist(possible_dates(x))
  .mean_value(y)
}

# Computes the mean instant of a vector of canonical date or date-time
# strings, via POSIXct when a time of day is present (so the time
# contributes to the average), or via Date otherwise.
.mean_value <- function(y) {
  if (any(grepl("[T ]", y)))
    return(format(mean(mdate_to_posixct(y)),
                  paste0("%Y-%m-%d", .dt_sep, "%H:%M:%S")))
  as.character(lubridate::as_date(mean(as.double(lubridate::as_date(y)))))
}

#' @rdname resolve_tendency
#' @export
vmean <- function(..., na.rm = TRUE) UseMethod("vmean")

#' @rdname resolve_tendency
#' @examples
#' vmean(d)
#' @export
vmean.mdate <- function(..., trim = 0, na.rm = TRUE, uncertain = "keep"){
  x <- list(...)[[1]]
  x <- .empty_to_na(x)
  x <- .handle_uncertain(x, uncertain, elementwise = TRUE)
  f <- .full_dates(x)
  out <- f$out
  out[f$rng] <- format(f$lo + (f$n - 1L) %/% 2L, "%Y-%m-%d")
  if (length(f$rest))
    out[f$rest] <- vapply(possible_dates(x[f$rest]), .mean_value,
                          FUN.VALUE = character(1))
  out
}

# Splits an mdate vector into the shapes that the v*() resolvers can handle
# without expanding each value: precise full dates, which resolve to
# themselves, and plain ranges of two full dates (years 1000-9999), which
# are summarised from their two ends. For a range of n days, the mean and
# the median both resolve to the start plus floor((n - 1) / 2) days, as the
# expanded computation does once the fractional day is dropped. All other
# values, including missing ones, are returned as `rest`, for the expanded
# computation, so that their behaviour does not change. `out` holds the
# precise values and NA elsewhere.
.full_dates <- function(x) {
  v <- as.character(unclass(x))
  day <- "[1-9][0-9]{3}-[0-9]{2}-[0-9]{2}"
  precise <- !is.na(v) & grepl(paste0("^", day, "$"), v)
  rng <- !is.na(v) & grepl(paste0("^", day, "\\.\\.", day, "$"), v)
  lo <- as.Date(substr(v[rng], 1, 10))
  n <- as.integer(as.Date(substr(v[rng], 13, 22)) - lo) + 1L
  ok <- !is.na(n) & n >= 1L
  rng[which(rng)[!ok]] <- FALSE
  out <- rep(NA_character_, length(v))
  out[precise] <- v[precise]
  list(out = out, rng = which(rng), lo = lo[ok], n = n[ok],
       rest = which(!precise & !rng), missing = is.na(v))
}

#' @rdname resolve_tendency
#' @export
modal <- function(..., na.rm = TRUE) UseMethod("modal")

#' @rdname resolve_tendency
#' @examples
#' modal(d)
#' @export
modal.mdate <- function(..., na.rm = TRUE, uncertain = "keep"){
  d <- list(...)[[1]]
  d <- .drop_empty(d)
  d <- .handle_uncertain(d, uncertain, elementwise = FALSE)
  .getmode(unlist(possible_dates(d)))
}

.getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#' @rdname resolve_tendency
#' @export
vmodal <- function(..., na.rm = TRUE) UseMethod("vmodal")

#' @rdname resolve_tendency
#' @examples
#' vmodal(d)
#' @export
vmodal.mdate <- function(..., na.rm = TRUE, uncertain = "keep"){

  d <- list(...)[[1]]

  d <- .empty_to_na(d)

  d <- .handle_uncertain(d, uncertain, elementwise = TRUE)
  d <- vapply(possible_dates(d), function(y) .getmode(y), character(1))
  d
}
