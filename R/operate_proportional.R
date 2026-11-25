#' Proportion of messy dates meeting logical test
#' @description
#'   These functions provide various proportional tests for messy date
#'   objects, complementing the strict logical comparisons in
#'   `?operate_inequalities`. Where a plain `<`/`>`/etc. comparison can only
#'   return `TRUE`, `FALSE`, or `NA` for a messy (imprecise) date, these
#'   functions instead report *what proportion* of the dates implied by
#'   `e1` satisfy the comparison against `e2`, by expanding both to their
#'   full sets of possible dates first.
#' @details
#'   Both kinds of set are expanded to their members, so `{}` ("all members
#'   of") and `[]` ("one member of") sets give the same proportion. For a `[]`
#'   set that proportion reads as a probability that the comparison holds,
#'   under the assumption that each candidate is equally likely, in the same
#'   way as for a range or an unspecified component. For a `{}` set it instead
#'   reads as the share of the recorded occurrences that satisfy it.
#'
#'   With `approx_range` more than 0, an approximate value (`~` or `%`) is
#'   widened as in `compatible_dates()`, and each of its days counts by the
#'   trapezoidal weight that `vrandom()` uses: 1 across the dates that the
#'   value names, falling to 0 over the dates that `approx_range` adds.
#'   An operator takes this argument when it is called as a function:
#'   `` `%l%`(x, y, approx_range = 3) ``.
#' @name operate_proportional
#' @param e1,e2 `mdate` or other class objects; must be of equal length.
#' @param approx_range The range by which to widen approximate values
#'   (`~` or `%`). By default 0, so that approximate values are not widened.
#' @param ... Arguments passed to the methods, such as `approx_range`.
#' @return A numeric vector, the same length as `e1` and `e2`, of
#'   proportions between 0 and 1.
NULL

#' @rdname operate_proportional
#' @usage `\%l\%`(e1, e2, ...)
#' @export
`%l%` <- function(e1, e2, ...) UseMethod("%l%")

#' @describeIn operate_proportional Tests proportion of dates in the first vector
#'   that precede the minimum in the second vector.
#' @examples
#'   as_messydate("2012-06") < as.Date("2012-06-02")
#'   as_messydate("2012-06") %l% as_messydate("2012-06-02")
#' @export
`%l%.mdate` <- function(e1, e2, ..., approx_range = 0) {
  .proportion(e1, e2, approx_range = approx_range,
    weighted = function(d, w, d2) sum(w[d < min(d2)]) / sum(w),
    fast = function(lo1, hi1, lo2, hi2, n) pmin(pmax(lo2 - lo1, 0), n) / n,
    slow = function(e1, e2) mapply(function(.x, .y) mean(.x < min(.y)),
                                   compatible_dates(e1), compatible_dates(e2),
                                   USE.NAMES = FALSE))
}

evalqOnLoad({
  registerS3method("%l%", "Date", `%l%.mdate`)
  registerS3method("%l%", "POSIXt", `%l%.mdate`)
})

#' @rdname operate_proportional
#' @usage `\%g\%`(e1, e2, ...)
#' @export
`%g%` <- function(e1, e2, ...) UseMethod("%g%")

#' @describeIn operate_proportional Tests proportion of dates in the first vector
#'   that follow the maximum in the second vector.
#' @export
#' @examples
#'   as_messydate("2012-06") > as.Date("2012-06-02")
#'   as_messydate("2012-06") %g% as_messydate("2012-06-02")
`%g%.mdate` <- function(e1, e2, ..., approx_range = 0) {
  .proportion(e1, e2, approx_range = approx_range,
    weighted = function(d, w, d2) sum(w[d > max(d2)]) / sum(w),
    fast = function(lo1, hi1, lo2, hi2, n) pmin(pmax(hi1 - hi2, 0), n) / n,
    slow = function(e1, e2) mapply(function(.x, .y) mean(.x > max(.y)),
                                   compatible_dates(e1), compatible_dates(e2),
                                   USE.NAMES = FALSE))
}

evalqOnLoad({
  registerS3method("%g%", "Date", `%g%.mdate`)
  registerS3method("%g%", "POSIXt", `%g%.mdate`)
})

#' @rdname operate_proportional
#' @usage `\%ge\%`(e1, e2, ...)
#' @export
`%ge%` <- function(e1, e2, ...) UseMethod("%ge%")

#' @describeIn operate_proportional Tests proportion of dates in the first vector
#'   that follow or are equal to the maximum in the second vector.
#' @export
#' @examples
#'   as_messydate("2012-06") >= as.Date("2012-06-02")
#'   as_messydate("2012-06") %ge% as_messydate("2012-06-02")
`%ge%.mdate` <- function(e1, e2, ..., approx_range = 0) {
  .proportion(e1, e2, approx_range = approx_range,
    weighted = function(d, w, d2) sum(w[d >= max(d2)]) / sum(w),
    fast = function(lo1, hi1, lo2, hi2, n) pmin(pmax(hi1 - hi2 + 1, 0), n) / n,
    slow = function(e1, e2) mapply(function(.x, .y) mean(.x >= max(.y)),
                                   compatible_dates(e1), compatible_dates(e2),
                                   USE.NAMES = FALSE))
}

evalqOnLoad({
  registerS3method("%ge%", "Date", `%ge%.mdate`)
  registerS3method("%ge%", "POSIXt", `%ge%.mdate`)
})

#' @rdname operate_proportional
#' @usage `\%le\%`(e1, e2, ...)
#' @export
`%le%` <- function(e1, e2, ...) UseMethod("%le%")

#' @describeIn operate_proportional Tests proportion of dates in the first vector
#'   that precede or are equal to the minimum in the second vector.
#' @export
#' @examples
#'   as_messydate("2012-06") <= as.Date("2012-06-02")
#'   as_messydate("2012-06") %le% "2012-06-02"
`%le%.mdate` <- function(e1, e2, ..., approx_range = 0) {
  .proportion(e1, e2, approx_range = approx_range,
    weighted = function(d, w, d2) sum(w[d <= min(d2)]) / sum(w),
    fast = function(lo1, hi1, lo2, hi2, n) pmin(pmax(lo2 - lo1 + 1, 0), n) / n,
    slow = function(e1, e2) mapply(function(.x, .y) mean(.x <= min(.y)),
                                   compatible_dates(e1), compatible_dates(e2),
                                   USE.NAMES = FALSE))
}

evalqOnLoad({
  registerS3method("%le%", "Date", `%le%.mdate`)
  registerS3method("%le%", "POSIXt", `%le%.mdate`)
})

#' @rdname operate_proportional
#' @usage `\%><\%`(e1, e2, ...)
#' @export
`%><%` <- function(e1, e2, ...) UseMethod("%><%")

#' @describeIn operate_proportional Tests proportion of dates in the first vector
#'   that are between the minimum and maximum dates in the second vector.
#' @export
#' @examples
#'   as_messydate("2012-06") %><% as_messydate("2012-06-15..2012-07-15")
`%><%.mdate` <- function(e1, e2, ..., approx_range = 0) {
  .proportion(e1, e2, approx_range = approx_range,
    weighted = function(d, w, d2) sum(w[d %in% d2]) / (sum(w) + 1),
    fast = function(lo1, hi1, lo2, hi2, n)
      pmax(pmin(hi1, hi2) - pmax(lo1, lo2) + 1, 0) / (n + 1),
    slow = function(e1, e2) mapply(function(.x, .y)
      length(intersect(compatible_dates(.x)[[1]], compatible_dates(.y)[[1]])) / (length(unlist(compatible_dates(.x))) + 1),
      e1, e2, USE.NAMES = FALSE))
}

evalqOnLoad({
  registerS3method("%><%", "Date", `%><%.mdate`)
  registerS3method("%><%", "POSIXt", `%><%.mdate`)
})

#' @rdname operate_proportional
#' @usage `\%>=<\%`(e1, e2, ...)
#' @export
`%>=<%` <- function(e1, e2, ...) UseMethod("%>=<%")

#' @describeIn operate_proportional Tests proportion of dates in the first vector that
#'   are between the minimum and maximum dates in the second vector, inclusive.
#' @export
#' @examples
#'   as_messydate("2012-06") %>=<% as_messydate("2012-06-15..2012-07-15")
`%>=<%.mdate` <- function(e1, e2, ..., approx_range = 0) {
  .proportion(e1, e2, approx_range = approx_range,
    weighted = function(d, w, d2) sum(w[d %in% d2]) / sum(w),
    fast = function(lo1, hi1, lo2, hi2, n)
      pmax(pmin(hi1, hi2) - pmax(lo1, lo2) + 1, 0) / n,
    slow = function(e1, e2) mapply(function(.x, .y)
      length(intersect(compatible_dates(.x)[[1]], compatible_dates(.y)[[1]])) / length(unlist(compatible_dates(.x))),
      e1, e2, USE.NAMES = FALSE))
}

evalqOnLoad({
  registerS3method("%>=<%", "Date", `%>=<%.mdate`)
  registerS3method("%>=<%", "POSIXt", `%>=<%.mdate`)
})

# Computes a proportion operator for each pair of elements.
# Where both elements are one unbroken run of days (a precise date, a year,
# a month, or a range of common-era dates), `fast()` computes the proportion
# from the first and last day of each run, with no list of days.
# The other elements (sets, unspecified digits, times, dates before the
# common era, open ranges, the empty set and NA) use `slow()`, which lists
# the possible dates of each element.
.proportion <- function(e1, e2, fast, slow, weighted, approx_range = 0) {
  if (length(e1) != length(e2))
    stop("Can only compare vectors of equal length.")
  .check_approx_range(approx_range)
  if (length(e1) == 0) return(numeric(0))
  if (!is_messydate(e1)) e1 <- suppressMessages(as_messydate(e1))
  if (!is_messydate(e2)) e2 <- suppressMessages(as_messydate(e2))
  # With approx_range, a pair with an approximate value uses the weighted
  # path: each day of e1 counts by its trapezoidal weight, as in vrandom(),
  # and e2 is all the days that it can be once widened.
  wt <- rep(FALSE, length(e1))
  if (approx_range > 0)
    wt <- .weights_apply(e1) | .weights_apply(e2)
  # A missing value on either side gives NA.
  miss <- is.na(e1) | is.na(e2)
  wt <- wt & !miss
  r1 <- .day_run(e1)
  r2 <- .day_run(e2)
  ok <- r1$ok & r2$ok & !wt & !miss
  out <- numeric(length(e1))
  out[miss] <- NA_real_
  if (any(ok)) {
    n <- r1$hi[ok] - r1$lo[ok] + 1
    out[ok] <- fast(r1$lo[ok], r1$hi[ok], r2$lo[ok], r2$hi[ok], n)
  }
  rest <- !ok & !wt & !miss
  if (any(rest)) out[rest] <- suppressMessages(slow(e1[rest], e2[rest]))
  if (any(wt))
    out[wt] <- mapply(function(a, b) {
      m1 <- .weighted_members(a, approx_range)
      m2 <- .weighted_members(b, approx_range)
      d2 <- m2$dates[m2$w > 0]
      if (!length(m1$dates) || !length(d2)) return(NA_real_)
      weighted(m1$dates, m1$w, d2)
    }, as.character(e1[wt]), as.character(e2[wt]), USE.NAMES = FALSE)
  out
}

# The first and last day, as day numbers, of each element that is one
# unbroken run of days, and which elements those are.
.day_run <- function(x) {
  s <- as.character(x)
  ok <- !is.na(s) &
    !stringi::stri_detect_regex(s, "[,{}\\[\\]X: T]|^-|\\.\\.-|^\\.\\.|\\.\\.$")
  ok[is.na(ok)] <- FALSE
  lo <- hi <- rep(NA_real_, length(s))
  if (any(ok)) {
    lo[ok] <- as.numeric(as.Date(vmin(x[ok]), FUN = identity))
    hi[ok] <- as.numeric(as.Date(vmax(x[ok]), FUN = identity))
    ok <- ok & !is.na(lo) & !is.na(hi) & lo <= hi
  }
  list(ok = ok, lo = lo, hi = hi)
}

# Proportion within a tolerance ---------------------------------------------

#' Proportion of day pairs within a tolerance
#' @description
#'   `date_by_intersecting()` is the proportion that goes with
#'   `date_is_intersecting()`.
#'   For each pair of values, it gives the share of pairs of compatible days,
#'   one from `x[i]` and one from `y[i]`, that are at most `within` days
#'   apart.
#'   If the two true dates are independent, and each is equally likely to be
#'   any of its compatible days, this is the probability that they are within
#'   `within` days of each other.
#'   It is 1 when every pair is within the tolerance (a certain match),
#'   between 0 and 1 when some are (a possible match), and 0 when none is.
#' @details
#'   With `approx_range` above 0, an approximate value (`~` or `%`) widens as
#'   in `compatible_dates()`, and each of its days counts by the trapezoidal
#'   weight that `vrandom()` uses, so that the share is a weighted share.
#'   Days, months, years and ranges of common-era dates are computed from
#'   their first and last days, without a list of days.
#'   Other values (sets, unspecified digits, open ranges) list their days.
#'   A missing value gives `NA`, and the empty set, `{}`, gives 0.
#'   A time of day is ignored, and dates before the common era give `NA`
#'   when `within` is above 0.
#' @inheritParams operate_statements
#' @name operate_within
#' @return A numeric vector of proportions between 0 and 1, of the common
#'   length of `x` and `y`.
#' @examples
#' # an exact day against a week-long range, and within a day of it
#' x <- as_messydate("2001-05-13..2001-05-19")
#' date_by_intersecting(x, as_messydate("2001-05-16"))
#' date_by_intersecting(x, as_messydate("2001-05-16"), within = 1)
#' # an approximate date, widened by three days with trapezoidal weights
#' date_by_intersecting(as_messydate("2001-05-16~"), as_messydate("2001-05-18"),
#'   within = 1, approx_range = 3)
#' @export
date_by_intersecting <- function(x, y, within = 0, approx_range = 0) {
  .check_within(within)
  .check_approx_range(approx_range)
  within <- floor(within)
  if (!is_messydate(x)) x <- suppressMessages(as_messydate(x))
  if (!is_messydate(y)) y <- suppressMessages(as_messydate(y))
  common <- vctrs::vec_recycle_common(x = x, y = y)
  x <- common$x
  y <- common$y
  n <- length(x)
  if (n == 0) return(numeric(0))
  out <- rep(NA_real_, n)
  miss <- is.na(x) | is.na(y)
  # Approximate values that widen, with approx_range, take the weighted path.
  wt <- rep(FALSE, n)
  if (approx_range > 0)
    wt <- (.approx_widens(as.character(x)) | .approx_widens(as.character(y))) & !miss
  r1 <- .day_run(x)
  r2 <- .day_run(y)
  fast <- r1$ok & r2$ok & !wt & !miss
  if (any(fast))
    out[fast] <- .prop_runs(r1$lo[fast], r1$hi[fast], r2$lo[fast], r2$hi[fast], within)
  rest <- !fast & !miss
  if (any(rest)) {
    vals <- unique(c(as.character(x[rest]), as.character(y[rest])))
    mem <- lapply(vals, .members, approx_range = approx_range)
    names(mem) <- vals
    out[rest] <- mapply(function(a, b) .prop_pairs(mem[[a]], mem[[b]], within),
                        as.character(x[rest]), as.character(y[rest]),
                        USE.NAMES = FALSE)
  }
  out
}

.check_within <- function(within) {
  if (!is.numeric(within) || length(within) != 1 || is.na(within) || within < 0)
    stop("`within` must be a single number of days, 0 or more.", call. = FALSE)
  invisible(within)
}

# The days of one value and their weights: trapezoidal weights for an
# approximate value that widens, and otherwise a weight of 1 for each of
# the days that compatible_dates() gives. The days with a weight above 0 are
# always the days of compatible_dates(y, approx_range).
.members <- function(y, approx_range = 0) {
  if (approx_range > 0 && isTRUE(.approx_widens(y))) {
    m <- .weighted_members(y, approx_range)
    keep <- m$w > 0
    return(list(dates = m$dates[keep], w = m$w[keep]))
  }
  d <- compatible_dates(new_messydate(y), approx_range = approx_range)[[1]]
  d <- d[!is.na(d)]
  list(dates = d, w = rep(1, length(d)))
}

# The weighted share of pairs of days (one from each value) that are at
# most `within` days apart. A value with no days gives 0.
.prop_pairs <- function(m1, m2, within) {
  if (length(m1$dates) == 0 || length(m2$dates) == 0) return(0)
  tot <- sum(m1$w) * sum(m2$w)
  if (within == 0) {
    v <- m2$w[match(m1$dates, m2$dates)]
    v[is.na(v)] <- 0
    return(sum(m1$w * v) / tot)
  }
  d1 <- suppressWarnings(as.numeric(as.Date(substr(m1$dates, 1, 10), optional = TRUE)))
  d2 <- suppressWarnings(as.numeric(as.Date(substr(m2$dates, 1, 10), optional = TRUE)))
  if (anyNA(d1) || anyNA(d2) || any(grepl("^-", c(m1$dates, m2$dates))))
    return(NA_real_)
  o <- order(d2)
  d2 <- d2[o]
  cw <- c(0, cumsum(m2$w[o]))
  # for each day of x, the weight of the days of y in [d - within, d + within]
  hi <- findInterval(d1 + within, d2)
  lo <- findInterval(d1 - within - 1, d2)
  sum(m1$w * (cw[hi + 1] - cw[lo + 1])) / tot
}

# The share of pairs (a, b), a in [l1, h1] and b in [l2, h2], all whole days,
# with |a - b| <= within, computed without listing the days.
# The count is H(-within) - H(within + 1), where H(s) counts the pairs with
# b >= a + s: for each a, all n2 days of y if a + s <= l2, h2 - a - s + 1 days
# if l2 < a + s <= h2, and none otherwise.
.prop_runs <- function(l1, h1, l2, h2, within) {
  n1 <- h1 - l1 + 1
  n2 <- h2 - l2 + 1
  H <- function(s) {
    full <- pmax(0, pmin(h1, l2 - s) - l1 + 1)
    p <- pmax(l1, l2 - s + 1)
    q <- pmin(h1, h2 - s)
    k <- pmax(0, q - p + 1)
    part <- ifelse(k > 0, k * (h2 - s + 1) - (p + q) * k / 2, 0)
    full * n2 + part
  }
  (H(-within) - H(within + 1)) / (n1 * n2)
}
