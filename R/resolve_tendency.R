#' Resolves messy dates into a central tendency
#' @description
#'   These functions resolve messydates by their central tendency.
#'   While the functions `mean()`, `median()`, and `modal()` expand *all*
#'   elements of the vector into one combined set of dates and summarise it
#'   to a single value (matching the usual behaviour of these generics),
#'   the `v*()` versions resolve each element separately and so return a
#'   vector of the same length as the input.
#' @details
#'   All of these functions work by calling `expand()` to enumerate the
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
#'   unspecified component. `vrandom()` draws a member uniformly in either
#'   case, which for a `[]` set is a draw from the candidates themselves.
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
  y <- unlist(expand(x))
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
    .median_value(.order_messy(unlist(expand(y))), na.rm = na.rm)
  }, FUN.VALUE = character(1))
  out
}

#' @rdname resolve_tendency
#' @param approx_range For `random()` and `vrandom()`, the range by which
#'   to widen approximate values (`~` or `%`).
#'   The range is in units of the component that each mark is written on,
#'   as in `expand()`.
#'   With the default of `0`, approximate values are not widened.
#'   With a positive value, the draw uses trapezoidal weights,
#'   see the section "Approximate and uncertain values".
#' @param uncertain_p For `random()` and `vrandom()`, the probability that an
#'   uncertain value (`?` or `%`) is correct.
#'   Give one number for all values, or one number for each value,
#'   for example a column that holds the reliability of each source.
#'   With probability `1 - uncertain_p`, `vrandom()` gives `NA` for an
#'   uncertain value, and `random()` leaves it out of the draw.
#'   The default of `1` treats uncertain values as correct.
#'   `uncertain_p = 0` is the same as `uncertain = "drop"`.
#' @section Approximate and uncertain values:
#'   The two annotations have different effects on a random draw.
#'
#'   An approximate value (`~`) can be outside the dates that it names.
#'   With a positive `approx_range`, the dates it names (the core) have a
#'   weight of 1, and the dates that `approx_range` adds on each side
#'   have a weight that falls linearly to 0.
#'   A range widens at each end by the marks of that end, so the weight is
#'   1 across the range and falls to 0 beyond an approximate end.
#'   The core and the full range are the "yolk" and the "egg" of the egg-yolk
#'   model of regions with indeterminate boundaries (Cohn and Gotts 1996).
#'   The linear fall between them is an assumption of this package.
#'
#'   An uncertain value (`?`) is a record that the source itself doubts.
#'   The doubt is about whether the record is correct at all,
#'   so it does not change the dates that the value can be.
#'   Instead, `uncertain_p` gives the probability that the record is correct.
#'   Repeated draws then show how much a result depends on doubtful records.
#' @references
#'   Cohn, A. G. and Gotts, N. M. (1996).
#'   "The 'egg-yolk' representation of regions with indeterminate boundaries".
#'   In P. A. Burrough and A. U. Frank (eds.),
#'   _Geographic Objects with Indeterminate Boundaries_, GISDATA 2,
#'   pp. 171-187. London: Taylor & Francis.
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
  y <- unlist(expand(x))
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
    out[f$rest] <- vapply(expand(x[f$rest]), .mean_value,
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
  .getmode(unlist(expand(d)))
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
  d <- vapply(expand(d), function(y) .getmode(y), character(1))
  d
}

#' @rdname resolve_tendency
#' @export
random <- function(..., na.rm = TRUE) UseMethod("random")

#' @rdname resolve_tendency
#' @examples
#' random(d)
#' @export
random.mdate <- function(..., na.rm = TRUE, approx_range = 0,
                         uncertain_p = 1) {
  x <- list(...)[[1]]
  x <- .drop_empty(x)
  # Each uncertain value is left out of this draw with probability
  # 1 - uncertain_p; the draw is then made from the members of the rest.
  miss <- .uncertain_misses(x, uncertain_p)
  x <- x[!miss]
  if (length(x) == 0) return(NA_character_)
  if (approx_range == 0) {
    y <- unlist(expand(x))
    w <- NULL
  } else {
    m <- lapply(as.character(x), .weighted_members, approx_range = approx_range)
    y <- unlist(lapply(m, `[[`, "dates"))
    w <- unlist(lapply(m, `[[`, "w"))
  }
  if (na.rm) {
    keep <- !is.na(y)
    y <- y[keep]
    if (!is.null(w)) w <- w[keep]
  }
  if (length(y) == 0) return(NA_character_)
  if (length(y) == 1) return(y)
  sample(y, 1, prob = w)
}

#' @export
random.character <- function(..., na.rm = TRUE) {
  y <- list(...)[[1]]
  # y <- suppressMessages(unlist(expand(x)))
  if(na.rm) y <- y[!is.na(y)]
  sample(y, 1)
}

#' @rdname resolve_tendency
#' @export
vrandom <- function(..., na.rm = TRUE) UseMethod("vrandom")

#' @rdname resolve_tendency
#' @examples
#' vrandom(d)
#' @export
vrandom.mdate <- function(..., na.rm = TRUE, approx_range = 0,
                          uncertain_p = 1) {

  x <- list(...)[[1]]

  x <- .empty_to_na(x)
  f <- .full_dates(x)
  out <- f$out
  # A range draws a uniform day, the same distribution as sampling its
  # expanded members.
  out[f$rng] <- format(f$lo + floor(stats::runif(length(f$lo)) * f$n),
                       "%Y-%m-%d")
  # Everything else (months, sets, annotations, times, BCE dates) is
  # expanded and sampled one value at a time, as before. Missing values
  # stay missing, as they do in vmin() and vmax().
  f$rest <- setdiff(f$rest, which(f$missing))
  out[f$rest] <- vapply(as.list(x[f$rest]), function(y){
    random(expand(y)[[1]], na.rm = na.rm)
  }, FUN.VALUE = character(1))
  # An approximate value draws from its trapezoidal weights instead.
  if (approx_range > 0) {
    apx <- which(!is.na(x) & date_is_approximate(x))
    out[apx] <- vapply(as.character(x[apx]), .weighted_draw, character(1),
                       approx_range = approx_range, USE.NAMES = FALSE)
  }
  # An uncertain value is taken to be wrong in this draw, and so gives NA,
  # with probability 1 - uncertain_p.
  out[.uncertain_misses(x, uncertain_p)] <- NA
  out

}
