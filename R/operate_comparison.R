# Inequalities ####

#' Logical operations on messy dates
#' @description
#'   These operators (`<`, `>`, `<=`, `>=`) compare `mdate` objects with
#'   each other, or with `Date`/`POSIXct`/`POSIXlt` objects, by comparing
#'   the range of dates each side could represent (its minimum and
#'   maximum), rather than requiring a single, precise value on both sides.
#'   A comparison returns `NA` wherever the two ranges overlap and the
#'   order cannot be determined; see the examples below. For a measure of
#'   *how much* of one side precedes or follows the other, rather than a
#'   strict `TRUE`/`FALSE`/`NA`, see `?operate_proportional`.
#' @param e1,e2 `mdate` or other class objects
#' @return A logical vector the same length as the longer of `e1` and `e2`.
#' @name operate_inequalities
NULL

#' @describeIn operate_inequalities tests whether the dates in the first vector precede
#'   the dates in the second vector.
#'   Returns `NA` when the date order can't be determined.
#' @examples
#' as_messydate("2012-06-02") > as.Date("2012-06-01") # TRUE
#' # 2012-06-XX could mean 2012-06-03, so unknown if it comes before 2012-06-02
#' as_messydate("2012-06-XX") < as.Date("2012-06-02") # NA
#' # But 2012-06-XX cannot be before 2012-06-01
#' as_messydate("2012-06-XX") >= as.Date("2012-06-01") # TRUE
#' # times of day are compared for two dates on the same day
#' as_messydate("2012-06-02 09:00") < as_messydate("2012-06-02 17:00") # TRUE
#' @export
`<.mdate` <- function(e1, e2) {
  if (.use_base_compare(e1, e2))
    return(.base_compare("<", e1, e2))
  if (!is_messydate(e1)) e1 <- as_messydate(e1)
  if (!is_messydate(e2)) e2 <- as_messydate(e2)
  ranges <- numeric_time_ranges(e1, e2)
  x <- rep(NA, if (length(e1) == 0 || length(e2) == 0) 0L else max(length(e1), length(e2)))
  x[ranges[["max1"]] < ranges[["min2"]]] <- TRUE
  x[ranges[["min1"]] > ranges[["max2"]]] <- FALSE
  x[ranges[["max1"]] == ranges[["min2"]]] <- FALSE
  x[ranges[["min1"]] == ranges[["max2"]]] <- FALSE
  x
}

# Quoth the {lubridate} team:
# Nothing else seems to work, only this sneaky trick.
evalqOnLoad({
  registerS3method("<", "Date", `<.mdate`)
  registerS3method("<", "POSIXt", `<.mdate`)
})

# Base R compares a Date with a POSIXct as raw numbers, days against
# seconds, so that mixed case keeps the mdate path, which compares them
# correctly. Any other comparison with no mdate on either side uses base R.
.use_base_compare <- function(e1, e2) {
  if (is_messydate(e1) || is_messydate(e2)) return(FALSE)
  !((inherits(e1, "Date") && inherits(e2, "POSIXt")) ||
      (inherits(e1, "POSIXt") && inherits(e2, "Date")))
}

# The package registers these methods for Date and POSIXt, so that a Date
# compared with an mdate dispatches here. When neither side is an mdate,
# and the two sides are not a Date and a POSIXt, compare as base R does: a character side is read as a Date or a POSIXct,
# and a POSIXlt side becomes a POSIXct.
.base_compare <- function(op, e1, e2) {
  posix <- inherits(e1, "POSIXt") || inherits(e2, "POSIXt")
  num <- function(x) {
    if (is.character(x)) x <- if (posix) as.POSIXct(x) else as.Date(x)
    if (inherits(x, "POSIXlt")) x <- as.POSIXct(x)
    as.vector(unclass(x))
  }
  out <- match.fun(op)(num(e1), num(e2))
  nm <- names(e1)
  if (is.null(nm) && length(e2) == length(out)) nm <- names(e2)
  if (!is.null(nm) && length(nm) == length(out)) names(out) <- nm
  out
}

numeric_time_ranges <- function(e1, e2) {
  # Both sides must be measured in the *same* unit, decided once for the
  # whole comparison (not independently per side): mixing seconds (for a
  # time-bearing endpoint) with days (for a plain date) would compare two
  # numbers on different scales and silently give the wrong answer, e.g.
  # treating "now" (~1.7 billion seconds since 1970) as later than
  # "9999-12-31" (~2.9 million days since 1970). `Sys.time() + Inf` (as
  # httr2 and others use for an unbounded retry deadline) resolves to
  # "9999-12-31" here (see as_messydate.POSIXct()'s Inf handling), so this
  # case is not just theoretical.
  bce <- .involves_bce(e1) || .involves_bce(e2)
  b1 <- .time_bounds(e1, bce)
  b2 <- .time_bounds(e2, bce)
  list(min1 = b1$min, max1 = b1$max, min2 = b2$min, max2 = b2$max)
}

# Whether an operand's resolved bounds include a date before the common era.
# na.rm = TRUE: an NA bound (e.g. from an NA POSIXct) is not itself BCE, and
# any() of an all-NA vector is NA rather than FALSE, which would otherwise
# make `if (bce)` in .time_bounds() error instead of just treating it as CE.
.involves_bce <- function(x) {
  if (!is_messydate(x)) return(FALSE)
  any(date_is_bce(c(as.character(vmin(x)), as.character(vmax(x)))), na.rm = TRUE)
}

# The numeric minimum and maximum instant implied by one side of a
# comparison. `<.mdate` and friends coerce both sides to `mdate` before
# calling this, so `x` is always an `mdate` here in practice; the `else`
# branch remains as a safeguard for direct callers.
# Measured in days (via `Date`, which supports BCE years) if `bce` is TRUE,
# since dates before the common era cannot be represented as `POSIXct`;
# otherwise measured in seconds (via `POSIXct`), so that a time of day, if
# present on either endpoint, contributes to the comparison rather than
# being truncated away (comparing dates alone would treat "2012-01-01 09:00"
# and "2012-01-01 17:00" as equal).
.time_bounds <- function(x, bce) {
  if (is_messydate(x)) {
    lo <- vmin(x)
    hi <- vmax(x)
    if (bce) {
      list(min = as.numeric(as.Date(lo)), max = as.numeric(as.Date(hi)))
    } else {
      list(min = as.numeric(mdate_to_posixct(as.character(lo))),
           max = as.numeric(mdate_to_posixct(as.character(hi))))
    }
  } else {
    v <- as.numeric(x)
    list(min = v, max = v)
  }
}

#' @describeIn operate_inequalities tests whether the dates in the first vector
#'   succeed the dates in the second vector.
#'   Returns `NA` when the date order can't be determined.
#' @export
`>.mdate` <- function(e1, e2) {
  if (.use_base_compare(e1, e2))
    return(.base_compare(">", e1, e2))
  if (!is_messydate(e1)) e1 <- as_messydate(e1)
  if (!is_messydate(e2)) e2 <- as_messydate(e2)
  ranges <- numeric_time_ranges(e1, e2)
  x <- rep(NA, if (length(e1) == 0 || length(e2) == 0) 0L else max(length(e1), length(e2)))
  x[ranges[["min1"]] > ranges[["max2"]]] <- TRUE
  x[ranges[["max1"]] < ranges[["min2"]]] <- FALSE
  x[ranges[["min1"]] == ranges[["max2"]]] <- FALSE
  x[ranges[["max1"]] == ranges[["min2"]]] <- FALSE
  x
}

evalqOnLoad({
  registerS3method(">", "Date", `>.mdate`)
  registerS3method(">", "POSIXt", `>.mdate`)
})

#' @describeIn operate_inequalities tests whether the dates in the first vector are
#'   equal to or precede the dates in the second vector.
#'   Returns `NA` when the date order can't be determined.
#' @export
`<=.mdate` <- function(e1, e2) {
  if (.use_base_compare(e1, e2))
    return(.base_compare("<=", e1, e2))
  if (!is_messydate(e1)) e1 <- as_messydate(e1)
  if (!is_messydate(e2)) e2 <- as_messydate(e2)
  ranges <- numeric_time_ranges(e1, e2)
  x <- rep(NA, if (length(e1) == 0 || length(e2) == 0) 0L else max(length(e1), length(e2)))
  x[ranges[["max1"]] <= ranges[["min2"]]] <- TRUE
  x[ranges[["min1"]] > ranges[["max2"]]] <- FALSE
  x
}

evalqOnLoad({
  registerS3method("<=", "Date", `<=.mdate`)
  registerS3method("<=", "POSIXt", `<=.mdate`)
})

#' @describeIn operate_inequalities tests whether the dates in the first vector are equal to
#'   or succeed the dates in the second vector.
#'   Returns `NA` when the date order can't be determined.
#' @export
`>=.mdate` <- function(e1, e2) {
  if (.use_base_compare(e1, e2))
    return(.base_compare(">=", e1, e2))
  if (!is_messydate(e1)) e1 <- as_messydate(e1)
  if (!is_messydate(e2)) e2 <- as_messydate(e2)
  ranges <- numeric_time_ranges(e1, e2)
  x <- rep(NA, if (length(e1) == 0 || length(e2) == 0) 0L else max(length(e1), length(e2)))
  x[ranges[["min1"]] >= ranges[["max2"]]] <- TRUE
  x[ranges[["max1"]] < ranges[["min2"]]] <- FALSE
  x
}

evalqOnLoad({
  registerS3method(">=", "Date", `>=.mdate`)
  registerS3method(">=", "POSIXt", `>=.mdate`)
})
