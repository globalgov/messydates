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
#'   widened as in `possible_dates()`, and each of its days counts by the
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
                                   possible_dates(e1), possible_dates(e2),
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
                                   possible_dates(e1), possible_dates(e2),
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
                                   possible_dates(e1), possible_dates(e2),
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
                                   possible_dates(e1), possible_dates(e2),
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
      length(intersect(possible_dates(.x)[[1]], possible_dates(.y)[[1]])) / (length(unlist(possible_dates(.x))) + 1),
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
      length(intersect(possible_dates(.x)[[1]], possible_dates(.y)[[1]])) / length(unlist(possible_dates(.x))),
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
