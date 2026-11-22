# Weights for approximate and uncertain values ####

# Approximate values ('~', and '%') -----------------------------------------
# An approximate value has a core, the dates the record names, and a wider
# support, the dates that expand(x, approx_range) adds around it. This is
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
  core <- expand(new_messydate(y))[[1]]
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
