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
#'   Averaging across the BCE/CE boundary, or between two BCE dates, is not
#'   currently supported: `median()` falls back to the earlier of the two
#'   middle values in that case, and `mean()`/`vmean()` may be unreliable for
#'   solely negative-year inputs (a documented limitation, not a supported
#'   feature).
#' @name coerce_tendency
#' @inheritParams coerce_extrema
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

#' @rdname coerce_tendency
#' @importFrom stats median
#' @examples
#' median(d)
#' @export
median.mdate <- function(..., na.rm = TRUE) {

  x <- list(...)[[1]]
  y <- unlist(expand(x))
  y <- .order_messy(y)
  .median_value(y, na.rm = na.rm)
}

.order_messy <- function(y){
  if(any(is_bce(y))){
    bcey <- y[is_bce(y)]
    cey <- y[!is_bce(y)]
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
  if (any(is_bce(c(lo, hi)))) return(lo)
  if (any(grepl("[T ]", c(lo, hi))))
    return(format(mean(mdate_to_posixct(c(lo, hi))),
                  paste0("%Y-%m-%d", .dt_sep, "%H:%M:%S")))
  as.character(mean(as.Date(c(lo, hi))))
}

#' @rdname coerce_tendency
#' @export
vmedian <- function(..., na.rm = TRUE) UseMethod("vmedian")

#' @rdname coerce_tendency
#' @importFrom stats median
#' @examples
#' vmedian(d)
#' @export
vmedian.mdate <- function(..., na.rm = TRUE) {

  x <- as.list(...)
  vapply(x, function(y) {
    .median_value(.order_messy(unlist(expand(y))), na.rm = na.rm)
  }, FUN.VALUE = character(1))
}

#' @rdname coerce_tendency
#' @param trim the fraction (0 to 0.5) of observations to be trimmed
#'   from each end of x before the mean is computed.
#'   Values of trim outside that range are taken as the nearest endpoint.
#' @importFrom lubridate as_date
#' @examples
#' mean(d)
#' @export
mean.mdate <- function(..., trim = 0, na.rm = TRUE) {

  x <- list(...)[[1]]
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

#' @rdname coerce_tendency
#' @export
vmean <- function(..., na.rm = TRUE) UseMethod("vmean")

#' @rdname coerce_tendency
#' @examples
#' vmean(d)
#' @export
vmean.mdate <- function(..., trim = 0, na.rm = TRUE) {
  x <- list(...)[[1]]
  vapply(expand(x), .mean_value, FUN.VALUE = character(1))
}

#' @rdname coerce_tendency
#' @export
modal <- function(..., na.rm = TRUE) UseMethod("modal")

#' @rdname coerce_tendency
#' @examples
#' modal(d)
#' @export
modal.mdate <- function(..., na.rm = TRUE) {
  d <- list(...)[[1]]
  .getmode(unlist(expand(d)))
}

.getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#' @rdname coerce_tendency
#' @export
vmodal <- function(..., na.rm = TRUE) UseMethod("vmodal")

#' @rdname coerce_tendency
#' @examples
#' vmodal(d)
#' @export
vmodal.mdate <- function(..., na.rm = TRUE) {

  d <- list(...)[[1]]
  d <- vapply(expand(d), function(y) .getmode(y), character(1))
  d
}

#' @rdname coerce_tendency
#' @export
random <- function(..., na.rm = TRUE) UseMethod("random")

#' @rdname coerce_tendency
#' @examples
#' random(d)
#' @export
random.mdate <- function(..., na.rm = TRUE) {
  x <- list(...)[[1]]
  y <- unlist(expand(x))
  if(na.rm) y <- y[!is.na(y)]
  sample(y, 1)
}

#' @export
random.character <- function(..., na.rm = TRUE) {
  y <- list(...)[[1]]
  # y <- suppressMessages(unlist(expand(x)))
  if(na.rm) y <- y[!is.na(y)]
  sample(y, 1)
}

#' @rdname coerce_tendency
#' @export
vrandom <- function(..., na.rm = TRUE) UseMethod("vrandom")

#' @rdname coerce_tendency
#' @examples
#' vrandom(d)
#' @export
vrandom.mdate <- function(..., na.rm = TRUE) {

  x <- as.list(...)
  vapply(x, function(y){
    random(expand(y)[[1]], na.rm = na.rm)
  }, FUN.VALUE = character(1))

}
