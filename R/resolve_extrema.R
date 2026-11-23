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
#'   unspecified `X` components) rather than via `possible_dates()`, which makes
#'   them considerably faster than resolving through the full expanded set.
#'   `min()` and `max()` further resolve a vector to a single, overall
#'   extremum (matching the usual behaviour of these generics), while
#'   `vmin()` and `vmax()` resolve each element separately.
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
#'   `possible_dates()`. 0 (the default) ignores the marks.
#'   With a positive value, `vmin()` and `vmax()` give the first and last
#'   dates of the widened support, the same dates that `possible_dates()`
#'   gives with the same `approx_range`, and that `vrandom()` can draw.
#' @importFrom stringi stri_detect_regex stri_replace_all_regex
#' @return A single scalar or vector of dates
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
  dates <- .remove_post(dates)
  dates <- .resolve_partial_x(dates, "min")
  dates <- .replace_earliest(dates)
  .as_resolved(dates)
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
  dates <- .remove_post(dates)
  dates <- .resolve_partial_x(dates, "min")
  dates <- .replace_earliest(dates)
  dates <- .as_resolved(dates)
  if(any(date_is_bce(dates)))
    dates[date_is_bce(dates)][order(as.character(dates[date_is_bce(dates)]),
                               decreasing = TRUE)][1] else
                                 dates[order(as.character(dates))[1]]
}

# Makes an mdate from resolved strings. A valid plain date (yyyy-mm-dd) is
# already in the notation that the parser writes, so it skips the parser.
# Every other string, including an impossible date, goes through mdate(),
# so that it is checked and written as before.
.as_resolved <- function(dates) {
  dates <- as.character(dates)
  plain <- !is.na(dates) & stringi::stri_detect_regex(dates, "^[0-9]{4}-[0-9]{2}-[0-9]{2}$")
  if (any(plain))
    plain[plain] <- !is.na(as.Date(dates[plain], optional = TRUE))
  if (all(plain)) return(new_messydate(dates))
  out <- dates
  out[!plain] <- as.character(mdate(dates[!plain]))
  new_messydate(out)
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
  dates <- unspecified_months(dates)
  dates <- .remove_pre(dates)
  dates <- .resolve_partial_x(dates, "max")
  dates <- .replace_latest(dates)
  .as_resolved(dates)
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
# annotated string give the same bounds as possible_dates() and vrandom().
# Date-times are left alone, as possible_dates() does not widen them by
# days. With approx_range = 0, `x` is returned unchanged.
.approx_to_range <- function(x, approx_range) {
  .check_approx_range(approx_range)
  if (approx_range == 0) return(x)
  v <- as.character(x)
  apx <- which(!is.na(v) & stringi::stri_detect_regex(v, "[~%]") &
                 !stringi::stri_detect_regex(v, "[T ][0-9X]{2}"))
  if (length(apx) == 0) return(x)
  pd <- possible_dates(new_messydate(v[apx]), approx_range = approx_range)
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
