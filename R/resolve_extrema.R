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
#' @importFrom stringi stri_detect_regex stri_replace_all_regex
#' @return A single scalar or vector of dates
#' @examples
#' d <- as_messydate(c("2008-03-25", "?2012-02-27", "2001-01?", "2001~",
#'   "2001-01-01..2001-02-02", "{2001-01-01,2001-02-02}",
#'   "{2001-01,2001-02-02}", "2008-XX-31", "-0050-01-01"))
#' d
#' # a precise date-time is returned unchanged
#' vmin(as_messydate("2012-01-01 14:30:00"))
#' @name resolve_extrema
NULL

#' @rdname resolve_extrema
#' @export
vmin <- function(..., na.rm = FALSE) UseMethod("vmin")

#' @rdname resolve_extrema
#' @examples
#' vmin(d)
#' @export
vmin.mdate <- function(..., na.rm = FALSE, uncertain = "keep"){
  d <- list(...)[[1]]
  d <- .empty_to_na(d)
  d <- .handle_uncertain(d, uncertain, elementwise = TRUE)
  dates <- d
  if(na.rm) dates <- stats::na.omit(d)
  dates <- stringi::stri_replace_all_regex(dates, "[~?%]", "")
  dates <- .remove_post(dates)
  dates <- .replace_earliest(dates)
  mdate(dates)
}

#' @rdname resolve_extrema
#' @examples
#' min(d)
#' @export
min.mdate <- function(..., na.rm = FALSE, uncertain = "keep"){
  d <- list(...)[[1]]
  d <- .drop_empty(d)
  d <- .handle_uncertain(d, uncertain, elementwise = FALSE)
  dates <- .na_for_reducer(d, na.rm)
  if (is.null(dates)) return(new_messydate(NA_character_))
  dates <- stringi::stri_replace_all_regex(dates, "[~?%]", "")
  dates <- .remove_post(dates)
  dates <- .replace_earliest(dates)
  dates <- mdate(dates)
  if(any(date_is_bce(dates)))
    dates[date_is_bce(dates)][order(as.character(dates[date_is_bce(dates)]),
                               decreasing = TRUE)][1] else
                                 dates[order(as.character(dates))[1]]
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
vmax.mdate <- function(..., na.rm = FALSE, uncertain = "keep"){
  d <- list(...)[[1]]
  d <- .empty_to_na(d)
  d <- .handle_uncertain(d, uncertain, elementwise = TRUE)
  dates <- d
  if(na.rm) dates <- stats::na.omit(d)
  dates <- stringi::stri_replace_all_regex(dates, "[~?%]", "")
  dates <- unspecified_months(dates)
  dates <- .remove_pre(dates)
  dates <- .replace_latest(dates)
  mdate(dates)
}

#' @rdname resolve_extrema
#' @examples
#' max(d)
#' @export
max.mdate <- function(..., na.rm = FALSE, uncertain = "keep"){
  d <- list(...)[[1]]
  d <- .drop_empty(d)
  d <- .handle_uncertain(d, uncertain, elementwise = FALSE)
  dates <- .na_for_reducer(d, na.rm)
  if (is.null(dates)) return(new_messydate(NA_character_))
  dates <- stringi::stri_replace_all_regex(dates, "[~?%]", "")
  dates <- unspecified_months(dates)
  dates <- .remove_pre(dates)
  dates <- .replace_latest(dates)
  dates <- mdate(dates)
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
