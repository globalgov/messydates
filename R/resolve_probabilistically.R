#' Resolves messy dates by a random draw
#' @description
#'   These functions resolve messydates by drawing one precise date at random
#'   from the dates that each value can be.
#'   `random()` expands *all* elements of the vector into one combined set of
#'   dates and draws a single value from it.
#'   `vrandom()` draws from each element separately, and so returns a vector
#'   of the same length as the input.
#'   Repeated draws give a distribution of results, which shows how much an
#'   analysis depends on the imprecision of the dates.
#' @details
#'   By default, each date that a value can be has the same chance of being
#'   drawn.
#'   For a `[]` ("one member of") set, this is a draw from the candidates
#'   themselves.
#'   For a `{}` ("all members of") set, it is a draw of one of the
#'   occurrences.
#'   The arguments `approx_range` and `uncertain_p` change the draw for
#'   approximate and uncertain values.
#' @return An `mdate` of precise dates or date-times.
#'   The `v*()` functions give one value for each element of `x`,
#'   with `NA` for a missing value and for the empty set, `{}`.
#'   The other functions give one value for the whole vector.
#' @name resolve_probabilistically
#' @inheritParams resolve_extrema
#' @param approx_range The range by which to widen approximate values (`~` or `%`).
#'   The range is in units of the component that each mark is written on,
#'   as in `possible_dates()`.
#'   With the default of `0`, approximate values are not widened.
#'   With a positive value, the draw uses trapezoidal weights,
#'   see the section "Approximate and uncertain values".
#' @param uncertain_p The probability that an uncertain value (`?` or `%`) is correct.
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
#' @examples
#' d <- as_messydate(c("2008-03-25", "?2012-02-27", "2001-01?", "2001~",
#'   "2001-01-01..2001-02-02", "{2001-01-01,2001-02-02}",
#'   "{2001-01,2001-02-02}", "2008-XX-31", "-0050-01-01"))
#' d
NULL

#' @rdname resolve_probabilistically
#' @export
random <- function(..., na.rm = TRUE) UseMethod("random")

#' @rdname resolve_probabilistically
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
  if (length(x) == 0) return(new_messydate(NA_character_))
  if (approx_range == 0) {
    y <- unlist(possible_dates(x))
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
  if (length(y) == 0) return(new_messydate(NA_character_))
  if (length(y) == 1) return(.as_resolved(y))
  .as_resolved(sample(y, 1, prob = w))
}

#' @export
random.character <- function(..., na.rm = TRUE) {
  y <- list(...)[[1]]
  # y <- suppressMessages(unlist(possible_dates(x)))
  if(na.rm) y <- y[!is.na(y)]
  sample(y, 1)
}

#' @rdname resolve_probabilistically
#' @export
vrandom <- function(..., na.rm = TRUE) UseMethod("vrandom")

#' @rdname resolve_probabilistically
#' @examples
#' vrandom(d)
#' @export
vrandom.mdate <- function(..., na.rm = TRUE, approx_range = 0,
                          uncertain_p = 1) {

  .check_dots(...)
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
    random(possible_dates(y)[[1]], na.rm = na.rm)
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
  .keep_names(.as_resolved(out), ...elt(1))

}

# Weights for approximate and uncertain values ----------------------------

# Approximate values ('~', and '%') -----------------------------------------
# An approximate value has a core, the dates the record names, and a wider
# support, the dates that possible_dates(x, approx_range) adds around it. This is
# the "egg-yolk" model of a region with an indeterminate boundary (Cohn and
# Gotts 1996): the core is the yolk, which certainly belongs to the region,
# and the support is the whole egg, which possibly belongs to it.
# The egg-yolk model says nothing about the dates between yolk and shell.
# Here they get a trapezoidal weight: a weight of 1 across the core, then a
# linear fall to 0 outside the support. A date one step outside the core on
# a side of L added dates gets a weight of 1 - 1 / (L + 1).

# Returns the dates that one value can be, and the weight of each.
# Values that are not approximate, or approx_range = 0, give weight 1.
# The support widens the core on each side by approx_range units of the
# finest component that each mark covers (see .approx_units()): a year for
# "~2012" or "~2012-06-15", a month for "2012-~06-15" or "2012-06~", and a
# day for "2012-06-~15" or "2012-06-15~".
# A range widens at each end by the marks of that end.
# Sets, open-ended ranges, values with unspecified digits, BCE dates, and
# date-times keep a flat weight for now.
.weighted_members <- function(y, approx_range = 0) {
  core <- possible_dates(new_messydate(y))[[1]]
  flat <- list(dates = core, w = rep(1, length(core)))
  if (approx_range == 0 || length(core) == 0 || !.approx_widens(y))
    return(flat)
  sup <- .approx_support(core, y, approx_range)
  lo <- sup$lo
  hi <- sup$hi
  from <- sup$from
  to <- sup$to
  d <- seq(from, to, by = "day")
  # Day numbers, as messydates methods for Date comparison are slower.
  n <- as.numeric(d)
  nlo <- as.numeric(lo)
  nhi <- as.numeric(hi)
  left <- nlo - as.numeric(from)
  right <- as.numeric(to) - nhi
  dist <- pmax(nlo - n, n - nhi, 0)
  side <- ifelse(n < nlo, left, right)
  w <- ifelse(dist == 0, 1, 1 - dist / (side + 1))
  # A core with gaps (e.g. "2012-XX-15") does not name the dates between.
  w[dist == 0 & !(format(d) %in% core)] <- 0
  list(dates = format(d), w = w)
}

# TRUE for each value whose support .approx_support() can compute:
# an approximate date (with '~' or '%'), or a closed range of two such
# dates, that is not a set, a date-time, a BCE date, or a date with
# unspecified digits. Open-ended ranges are not widened.
.approx_widens <- function(y) {
  !is.na(y) & stringi::stri_detect_regex(y, "[~%]") &
    !stringi::stri_detect_regex(y, "[{}\\[\\],T X]|^-|\\.\\.-") &
    (!stringi::stri_detect_regex(y, "\\.\\.") |
       stringi::stri_detect_regex(y, "^[^.]+\\.\\.[^.]+$"))
}

# The support of one approximate value: its core, from `lo` to `hi`,
# widened on each side by approx_range units of the finest component that
# each mark covers (see .approx_units()).
# `core` holds the dates of the value without its annotations.
# For a range, the marks of the first end widen the start and the marks of
# the second end widen the end: "2012-03-~01..2012-04-15" widens only the
# start, by approx_range days.
.approx_support <- function(core, y, approx_range) {
  ends <- strsplit(y, "..", fixed = TRUE)[[1]]
  if (length(ends) == 1) ends <- c(y, y)
  cd <- as.Date(core)
  lo <- min(cd)
  hi <- max(cd)
  from <- .approx_shift(lo, ends[1], -approx_range)
  # The end is counted from the day after the core, so that "2012-06~"
  # with approx_range = 1 ends on 31 August 2013, not 30 August.
  to <- .approx_shift(hi + 1, ends[2], approx_range) - 1
  list(lo = lo, hi = hi, from = from, to = to)
}

# Shifts a date by k units of each component that .approx_units() gives.
# A shift to a day that does not exist (31 February) rolls back to the
# last day of the month.
.approx_shift <- function(d, y, k) {
  for (u in c("year", "month", "day")[.approx_units(y)])
    d <- lubridate::add_with_rollback(d, lubridate::period(k, u))
  d
}

# The components (year, month, day) by which the '~' and '%' marks in a
# value widen it: the finest component that each mark covers.
# By ISO 8601-2:2019, a mark after a component covers that component and all
# components to its left ("2004-06~" covers year and month), and a mark
# before a component covers only that component ("2004-~06" covers the
# month). A mark after the whole date says that the date, as written, is
# approximate, so it widens at the precision of the date ("2004-06-11~" by
# days, "2004-06~" by months), not by each component at once.
# Each mark gives the component next to it, so the finest component that a
# mark covers is the component that it is written on.
.approx_units <- function(y) {
  parts <- strsplit(y, "-", fixed = TRUE)[[1]]
  units <- rep(FALSE, 3)
  for (i in seq_len(min(length(parts), 3)))
    if (grepl("^[~%]|[~%]$", parts[i])) units[i] <- TRUE
  units
}

# Draws one date from the weighted members of one value.
.weighted_draw <- function(y, approx_range = 0) {
  m <- .weighted_members(y, approx_range)
  if (length(m$dates) == 0) return(NA_character_)
  if (length(m$dates) == 1) return(m$dates)
  sample(m$dates, 1, prob = m$w)
}

# Uncertain values ('?', and '%') -----------------------------------------
# An uncertain value is a record that the source itself doubts. The doubt is
# about whether the record is correct at all, not about which dates it names,
# so it does not change the support. Instead each uncertain value has a
# probability, uncertain_p, that it is correct.

.check_uncertain_p <- function(uncertain_p, n) {
  if (!is.numeric(uncertain_p) || any(is.na(uncertain_p)) ||
      any(uncertain_p < 0 | uncertain_p > 1))
    stop("`uncertain_p` must be a number from 0 to 1.", call. = FALSE)
  if (!length(uncertain_p) %in% c(1, n))
    stop("`uncertain_p` must have length 1 or the length of the dates (",
         n, ").", call. = FALSE)
  rep_len(uncertain_p, n)
}

# For a random draw: TRUE where an uncertain value is taken to be wrong in
# this draw, with probability 1 - uncertain_p.
.uncertain_misses <- function(x, uncertain_p) {
  p <- .check_uncertain_p(uncertain_p, length(x))
  unc <- !is.na(x) & date_is_uncertain(x)
  unc & stats::runif(length(x)) >= p
}

# For a deterministic resolver: with uncertain = "drop", an element-wise
# resolver gives NA for each uncertain value, and a resolver over the whole
# vector leaves uncertain values out.
.handle_uncertain <- function(x, uncertain = c("keep", "drop"),
                              elementwise = TRUE) {
  uncertain <- match.arg(uncertain)
  if (uncertain == "keep") return(x)
  unc <- !is.na(x) & date_is_uncertain(x)
  if (!any(unc)) return(x)
  if (elementwise) {
    x[unc] <- NA
    return(x)
  }
  x <- x[!unc]
  if (length(x) == 0) x <- new_messydate(NA_character_)
  x
}
