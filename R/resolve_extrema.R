#' Resolves messy dates into an extrema
#' @description
#'   This collection of S3 methods 'resolve' messy dates into a single date
#'   according to some explicit bias,
#'   such as returning the minimum or maximum date,
#'   the mean, median, or modal date,
#'   or a random date from among the possible resolutions for each messy date.
#'   If the date is not 'messy' (i.e. has no annotations)
#'   then just that precise date is returned.
#'   This can be useful for various descriptive or inferential projects.
#' @details
#'   `vmin()`/`min()` and `vmax()`/`max()` work directly on the annotated
#'   string (dropping `~`/`?`/`%` and, for `vmax()`/`max()`, filling in
#'   unspecified `X` components) rather than via `compatible_dates()`, which makes
#'   them considerably faster than resolving through the full expanded set.
#'   `min()` and `max()` further resolve a vector to a single, overall
#'   extremum (matching the usual behaviour of these generics), while
#'   `vmin()` and `vmax()` resolve each element separately.
#'
#' @section What min() and max() mean for messy dates:
#'   For a number, `min()` and `max()` give the smallest or largest value in
#'   a vector. For an `mdate`, they are resolution functions: `min()` gives
#'   the earliest date that any element of the vector can be, and `max()`
#'   gives the latest. The result is a precise date, and it need not be an
#'   element of the vector:
#'   `min(as_messydate(c("2012", "2011-06")))` gives `"2011-06-01"`, the
#'   first day of the earliest element, not the element `"2011-06"`.
#'   `range()` gives `min()` and `max()` together.
#'
#'   This differs from ordering. `sort()` and `order()` order the elements
#'   themselves, and `<` gives `NA` when two elements overlap, because it
#'   cannot say which comes first. `min()` and `max()` do not compare the
#'   elements; they resolve the union of their possible dates.
#'   The package keeps the base names, and does not use prefixed names, because
#'   the meaning agrees with the base generics: each reduces a vector to one
#'   value of its class.
#'
#'   Dates that carry a time of day are already precise and so pass through
#'   these functions unchanged, keeping their time; a time of day plays no
#'   further role in choosing the minimum or maximum.
#' @param ... a mdate object
#' @param na.rm Should NAs be removed? FALSE by default.
#'   As in base R, with `na.rm = FALSE` one `NA` makes the result of
#'   `min()` or `max()` `NA`.
#' @param uncertain What to do with uncertain values (`?` or `%`), which the
#'   source itself doubts.
#'   `"keep"` (the default) resolves them as other values.
#'   `"drop"` gives `NA` for them in the element-wise `v*()` functions,
#'   and leaves them out of the functions that resolve the whole vector,
#'   such as `min()`.
#'   For random draws, see `uncertain_p` in `vrandom()`.
#' @param approx_range Range to widen approximate values (`~` or `%`) by,
#'   in units of the component that each mark covers, as in
#'   `compatible_dates()`. 0 (the default) ignores the marks.
#'   With a positive value, `vmin()` and `vmax()` give the first and last
#'   dates of the widened support, the same dates that `compatible_dates()`
#'   gives with the same `approx_range`, and that `vrandom()` can draw.
#' @importFrom stringi stri_detect_regex stri_replace_all_regex
#' @return An `mdate` of precise dates or date-times.
#'   The `v*()` functions give one value for each element of `x`,
#'   with `NA` for a missing value and for the empty set, `{}`.
#'   The other functions give one value for the whole vector.
#' @examples
#' d <- as_messydate(c("2008-03-25", "?2012-02-27", "2001-01?", "2001~",
#'   "2001-01-01..2001-02-02", "{2001-01-01,2001-02-02}",
#'   "{2001-01,2001-02-02}", "2008-XX-31", "-0050-01-01"))
#' d
#' # a precise date-time is returned unchanged
#' vmin(as_messydate("2012-01-01 14:30:00"))
#' # an approximate date widens by approx_range
#' vmin(as_messydate("2001-05-16~"), approx_range = 3)
#' @name resolve_extrema
NULL

#' @rdname resolve_extrema
#' @export
vmin <- function(..., na.rm = FALSE) UseMethod("vmin")

#' @rdname resolve_extrema
#' @examples
#' vmin(d)
#' @export
vmin.mdate <- function(..., na.rm = FALSE, uncertain = "keep", approx_range = 0){
  .check_dots(...)
  d <- list(...)[[1]]
  d <- .empty_to_na(d)
  d <- .handle_uncertain(d, uncertain, elementwise = TRUE)
  d <- .approx_to_range(d, approx_range)
  dates <- d
  if(na.rm) dates <- stats::na.omit(d)
  dates <- stringi::stri_replace_all_regex(dates, "[~?%]", "")
  dates <- .resolve_sets(dates, "min")
  dates <- .drop_trailing_x(dates)
  dates <- .remove_post(dates)
  dates <- .resolve_partial_x(dates, "min")
  dates <- .replace_earliest(dates)
  .keep_names(.as_resolved(dates), ...elt(1))
}

#' @rdname resolve_extrema
#' @examples
#' min(d)
#' @export
min.mdate <- function(..., na.rm = FALSE, uncertain = "keep", approx_range = 0){
  d <- list(...)[[1]]
  d <- .drop_empty(d)
  d <- .handle_uncertain(d, uncertain, elementwise = FALSE)
  d <- .approx_to_range(d, approx_range)
  dates <- .na_for_reducer(d, na.rm)
  if (is.null(dates)) return(new_messydate(NA_character_))
  dates <- stringi::stri_replace_all_regex(dates, "[~?%]", "")
  dates <- .resolve_sets(dates, "min")
  dates <- .drop_trailing_x(dates)
  dates <- .remove_post(dates)
  dates <- .resolve_partial_x(dates, "min")
  dates <- .replace_earliest(dates)
  dates <- .as_resolved(dates)
  if(any(date_is_bce(dates)))
    dates[date_is_bce(dates)][order(as.character(dates[date_is_bce(dates)]),
                               decreasing = TRUE)][1] else
                                 dates[order(as.character(dates))[1]]
}

#' @rdname resolve_extrema
#' @examples
#' range(d)
#' @export
range.mdate <- function(..., na.rm = FALSE, uncertain = "keep", approx_range = 0) {
  lo <- min.mdate(..., na.rm = na.rm, uncertain = uncertain,
                  approx_range = approx_range)
  hi <- max.mdate(..., na.rm = na.rm, uncertain = uncertain,
                  approx_range = approx_range)
  vctrs::vec_c(lo, hi)
}

# Makes an mdate from resolved strings. A value that is already in the
# canonical notation that the parser writes skips the parser: a valid date
# (yyyy-mm-dd, also before the common era), optionally with a valid time of
# day and offset (" hh:mm", " hh:mm:ss", fractional seconds, "Z", "+hh:mm").
# A missing value stays missing. Every other string, including an
# impossible date or time, goes through mdate(), so that it is checked and
# written as before.
.as_resolved <- function(dates) {
  dates <- as.character(dates)
  done <- is.na(dates) | .is_canonical_resolved(dates)
  if (all(done)) return(new_messydate(dates))
  out <- dates
  out[!done] <- as.character(mdate(dates[!done]))
  new_messydate(out)
}

.canonical_resolved_rx <- paste0(
  "^(-?)([0-9]{4})-([0-9]{2})-([0-9]{2})",
  "(?: ([0-9]{2}):([0-9]{2})(?::([0-9]{2})(?:\\.[0-9]+)?)?",
  "(Z|[+-]([0-9]{2}):([0-9]{2}))?)?$")

# TRUE for each string in the canonical notation with valid components.
.is_canonical_resolved <- function(dates) {
  out <- rep(FALSE, length(dates))
  m <- stringi::stri_match_first_regex(dates, .canonical_resolved_rx)
  hit <- !is.na(m[, 1])
  if (!any(hit)) return(out)
  m <- m[hit, , drop = FALSE]
  num <- function(k) suppressWarnings(as.integer(m[, k]))
  yr <- num(3) * ifelse(m[, 2] == "-", -1L, 1L)
  mo <- num(4)
  dy <- num(5)
  ok <- mo >= 1 & mo <= 12 & dy >= 1
  ok[ok] <- dy[ok] <= days_in(yr[ok], mo[ok])
  ok <- ok & (is.na(num(6)) | num(6) <= 23) & (is.na(num(7)) | num(7) <= 59) &
    (is.na(num(8)) | num(8) <= 59) &
    (is.na(num(10)) | num(10) <= 14) & (is.na(num(11)) | num(11) <= 59)
  out[hit] <- ok %in% TRUE
  out
}

# Gives the result of an element-wise resolver the names of its input,
# as base R does for pmin() and pmax(). With na.rm = TRUE, vmin() and
# vmax() leave out missing values, and the result then has no names.
.keep_names <- function(out, x) {
  if (!is.null(names(x)) && length(out) == length(x)) names(out) <- names(x)
  out
}

# Drops an unspecified month or day at the end of a date, at the end of
# each range end, and at the end of each set member: "1993-01-XX" has the
# same earliest and latest day as "1993-01", and "1993-XX-XX" as "1993".
# An unspecified component between known ones ("1993-XX-15") is kept.
.drop_trailing_x <- function(dates) {
  stringi::stri_replace_all_regex(dates, "-XX(-XX)?(?=$|\\.\\.|,|\\}|\\]| )", "")
}

# Resolves each set ({} or []) to its earliest or latest day. Each member
# is resolved on its own, as a value of its own, and the members are then
# compared by day, so that the order of the members and a member with
# month or year precision do not change the result.
.resolve_sets <- function(dates, bound) {
  dates <- as.character(dates)
  hit <- !is.na(dates) & stringi::stri_detect_regex(dates, "^[{\\[].*[}\\]]$")
  if (!any(hit)) return(dates)
  dates[hit] <- vapply(dates[hit], function(v) {
    members <- stringi::stri_split_fixed(substr(v, 2, nchar(v) - 1), ",")[[1]]
    members <- members[nzchar(members)]
    if (!length(members)) return(NA_character_)
    m <- new_messydate(members)
    res <- as.character(if (bound == "min") vmin(m) else vmax(m))
    num <- as.numeric(as.Date(new_messydate(strip_times(res)), FUN = identity))
    if (all(is.na(num))) return(NA_character_)
    res[if (bound == "min") which.min(num) else which.max(num)]
  }, character(1), USE.NAMES = FALSE)
  dates
}

# Resolves a value whose month or day has one unspecified digit
# ("2012-1X") to its earliest or latest valid day.
.resolve_partial_x <- function(dates, bound) {
  hit <- .has_partial_x(dates)
  if (any(hit))
    dates[hit] <- vapply(dates[hit], function(v) {
      days <- .partial_x_days(v)
      if (!length(days)) return(NA_character_)
      if (bound == "min") days[1] else days[length(days)]
    }, character(1), USE.NAMES = FALSE)
  dates
}

.remove_post <- function(dates){
  dates <- stringi::stri_replace_all_regex(dates, "\\.\\.$|,.*$|\\{|\\[", "")
  dates <- stringi::stri_replace_all_regex(dates, "^(.+)\\.\\..*$", "$1")
  dates <- stringi::stri_replace_all_regex(dates, "\\.\\.", "")
  dates
}

# Resolves a year that holds 'X' ("XX29", "192X") to its earliest or latest
# possible date, leaving other values alone.
.resolve_x_years <- function(dates, bound) {
  hasx <- !is.na(dates) & stringi::stri_detect_regex(dates, "^-?[0-9]*X[0-9X]*(-|$)")
  if (any(hasx))
    dates[hasx] <- vapply(dates[hasx], resolve_x_year, character(1),
                          bound = bound, USE.NAMES = FALSE)
  dates
}

.replace_earliest <- function(dates){
  dates <- .resolve_x_years(dates, "min")
  dates <- stringi::stri_replace_last_regex(dates,
                                            "XX", "01")
  dates <- stringi::stri_replace_last_regex(dates,
                                            "^(.*[:digit:]{4})$", "$1-01-01")
  dates <- stringi::stri_replace_last_regex(dates,
                                            "^(.*[:digit:]{4})-([:digit:]{2})$", "$1-$2-01")
  # dates <- stringi::stri_replace_last_regex(dates,
  #                                           "^-([:digit:]{4})-([:digit:]{2})$", "-$1-$2-01")
  dates
}

#' @rdname resolve_extrema
#' @export
vmax <- function(..., na.rm = FALSE) UseMethod("vmax")

#' @rdname resolve_extrema
#' @examples
#' vmax(d)
#' @export
vmax.mdate <- function(..., na.rm = FALSE, uncertain = "keep", approx_range = 0){
  .check_dots(...)
  d <- list(...)[[1]]
  d <- .empty_to_na(d)
  d <- .handle_uncertain(d, uncertain, elementwise = TRUE)
  d <- .approx_to_range(d, approx_range)
  dates <- d
  if(na.rm) dates <- stats::na.omit(d)
  dates <- stringi::stri_replace_all_regex(dates, "[~?%]", "")
  dates <- .resolve_sets(dates, "max")
  dates <- .drop_trailing_x(dates)
  dates <- unspecified_months(dates)
  dates <- .remove_pre(dates)
  dates <- .resolve_partial_x(dates, "max")
  dates <- .replace_latest(dates)
  .keep_names(.as_resolved(dates), ...elt(1))
}

#' @rdname resolve_extrema
#' @examples
#' max(d)
#' @export
max.mdate <- function(..., na.rm = FALSE, uncertain = "keep", approx_range = 0){
  d <- list(...)[[1]]
  d <- .drop_empty(d)
  d <- .handle_uncertain(d, uncertain, elementwise = FALSE)
  d <- .approx_to_range(d, approx_range)
  dates <- .na_for_reducer(d, na.rm)
  if (is.null(dates)) return(new_messydate(NA_character_))
  dates <- stringi::stri_replace_all_regex(dates, "[~?%]", "")
  dates <- .resolve_sets(dates, "max")
  dates <- .drop_trailing_x(dates)
  dates <- unspecified_months(dates)
  dates <- .remove_pre(dates)
  dates <- .resolve_partial_x(dates, "max")
  dates <- .replace_latest(dates)
  dates <- .as_resolved(dates)
  if(all(date_is_bce(dates), na.rm = TRUE))
    dates[order(dates, decreasing = TRUE)][1] else
      dates[!date_is_bce(dates)][order(as.character(dates[!date_is_bce(dates)]),
                                  decreasing = TRUE)][1]

}

.remove_pre <- function(dates){
  dates <- stringi::stri_replace_all_regex(dates, "^\\.\\.|^.*,|\\}|\\]", "")
  dates <- stringi::stri_replace_all_regex(dates, "^.*\\.\\.(.+)$", "$1")
  dates <- stringi::stri_replace_all_regex(dates, "\\.\\.", "")
  dates
}

.replace_latest <- function(dates){
  dates <- .resolve_x_years(dates, "max")
  dates <- stringi::stri_replace_last_regex(dates,
                                            "^(.*[:digit:]{4})$", "$1-12-31")
  dates <- stringi::stri_replace_last_regex(dates,
                                            "-XX-", "-12-")
  dates
}


# Applies the base R rule for NA to a reducer such as min() or max().
# With na.rm = FALSE, one NA makes the result NA.
# With na.rm = TRUE, the NA values are left out.
# NULL means that the result is NA.
.na_for_reducer <- function(x, na.rm) {
  miss <- is.na(x)
  if (any(miss) && !na.rm) return(NULL)
  x <- x[!miss]
  if (length(x) == 0) return(NULL)
  x
}

# The empty set, '{}', names no dates. An element-wise resolver gives NA for
# it, as there is no date to resolve to.
.empty_to_na <- function(x) {
  empty <- is_empty_notation(x)
  if (any(empty)) x[empty] <- NA
  x
}

# A resolver over the whole vector resolves the union of its elements, and
# the empty set adds nothing to a union, so it is left out.
# If no element remains, the result is NA.
.drop_empty <- function(x) {
  empty <- is_empty_notation(x)
  if (!any(empty)) return(x)
  x <- x[!empty]
  if (length(x) == 0) x <- new_messydate(NA_character_)
  x
}

# Replaces each approximate value (with '~' or '%') by the range of its
# support under approx_range, so that the resolvers that work on the
# annotated string give the same bounds as compatible_dates() and vrandom().
# Date-times are left alone, as compatible_dates() does not widen them by
# days. With approx_range = 0, `x` is returned unchanged.
.approx_to_range <- function(x, approx_range) {
  .check_approx_range(approx_range)
  if (approx_range == 0) return(x)
  v <- as.character(x)
  apx <- which(!is.na(v) & stringi::stri_detect_regex(v, "[~%]") &
                 !stringi::stri_detect_regex(v, "[T ][0-9X]{2}"))
  if (length(apx) == 0) return(x)
  pd <- compatible_dates(new_messydate(v[apx]), approx_range = approx_range)
  v[apx] <- vapply(pd, function(d) {
    d <- .order_messy(d[!is.na(d)])
    if (length(d) == 0) return(NA_character_)
    if (length(d) == 1) return(d)
    paste0(d[1], "..", d[length(d)])
  }, character(1))
  new_messydate(v)
}

.check_approx_range <- function(approx_range) {
  if (!is.numeric(approx_range) || length(approx_range) != 1 ||
      is.na(approx_range) || approx_range < 0)
    stop("`approx_range` must be a single number of 0 or more.", call. = FALSE)
  invisible(approx_range)
}

# An element-wise resolver takes one mdate vector. Any further argument
# that it does not name is an error, so that a misspelled argument, such
# as `aprox_range`, is not silently ignored.
.check_dots <- function(...) {
  n <- ...length()
  if (n <= 1) return(invisible())
  nm <- names(list(...))[-1]
  if (is.null(nm)) nm <- rep("", n - 1)
  nm[is.na(nm) | nm == ""] <- "an unnamed argument"
  stop("Unused argument", if (n > 2) "s", ": ", paste(nm, collapse = ", "),
       ". Element-wise resolvers take one mdate vector.", call. = FALSE)
}
