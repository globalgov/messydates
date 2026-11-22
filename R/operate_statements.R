# Statements ####

#' Logical statements on messy dates
#' @description
#'   These functions provide various logical statements about messy date objects.
#' @name operate_statements
#' @param x,y `mdate` or other class objects
#' @return A logical vector the same length as the `mdate` passed.
NULL

#' @describeIn operate_statements tests whether the object inherits the `mdate` class.
#'   If more rigorous validation is required, see `validate_messydate()`.
#' @examples
#' is_messydate(as_messydate("2012-01-01"))
#' is_messydate(as.Date("2012-01-01"))
#' @export
is_messydate <- function(x) {
  inherits(x, "mdate")
}

#' @describeIn operate_statements tests whether there is any intersection between
#'   two messy dates, leveraging `intersect()`.
#' @examples
#' is_intersecting(as_messydate("2012-01"),
#' as_messydate("2012-01-01..2012-02-22"))
#' is_intersecting(as_messydate("2012-01"),
#' as_messydate("2012-02-01..2012-02-22"))
#' @export
is_intersecting <- function(x, y) {
  length(intersect(unlist(expand(x)), unlist(expand(y)))) > 0
}

#' @describeIn operate_statements tests whether one or more messy date can be found
#'   within a messy date range or set.
#' @examples
#' is_subset(as_messydate("2012-01-01"), as_messydate("2012-01"))
#' is_subset(as_messydate("2012-01-01..2012-01-03"), as_messydate("2012-01"))
#' is_subset(as_messydate("2012-01-01"), as_messydate("2012-02"))
#' @export
is_subset <- function(x, y) {
  x <- as.character(expand(x)[[1]])
  y <- as.character(expand(y)[[1]])
  any(is.element(x, y))
}

#' @describeIn operate_statements tests whether two dates contain similar components.
#'   This can be useful for identifying dates that may be typos of one another.
#' @examples
#' date_is_similar(as_messydate("2012-06-02"), as_messydate("2012-02-06"))
#' date_is_similar(as_messydate("2012-06-22"), as_messydate("2012-02-06"))
#' @export
date_is_similar <- function(x, y) {
  # Coerce to mdate so the component accessors dispatch to their `.mdate`
  # methods (rather than {lubridate}'s) even when passed bare strings.
  if (!is_messydate(x)) x <- as_messydate(x)
  if (!is_messydate(y)) y <- as_messydate(y)
  year(x) == year(y) & month(x) == day(y) & day(x) == month(y)
}

#' @describeIn operate_statements tests whether a date (or date-time) is
#'   precise, i.e. a full "yyyy-mm-dd", optionally with a time of day (down
#'   to the hour, minute, or second) and time zone. Non-precise dates
#'   contain markers that they are approximate (i.e. ~), unreliable (i.e.
#'   ?), are incomplete (e.g. year or year-month only, or a date with an
#'   unspecified `X` component), or are date ranges and sets.
#' @examples
#' date_is_precise(as_messydate(c("2012-06-02", "2012-06")))
#' date_is_precise(as_messydate(c("2012-06-02 14:30:00", "2012-06-02 ~14")))
#' @export
date_is_precise <- function(x) {
  stringi::stri_detect_regex(
    x,
    paste0("^-?[0-9]{4}-[0-9]{2}-[0-9]{2}",
           "([T ][0-9]{2}(:[0-9]{2}(:[0-9]{2}(\\.[0-9]+)?)?)?",
           "(Z|[+-][0-9]{2}:[0-9]{2})?)?$"))
}

#' @describeIn operate_statements tests whether a date is uncertain (i.e. contains ?).
#' @examples
#' date_is_uncertain(as_messydate(c("2012-06-02", "2012-06-02?")))
#' @export
date_is_uncertain <- function(x) {
  stringi::stri_detect_regex(x, "\\?|\\%")
}

#' @describeIn operate_statements tests whether a date is approximate (i.e. contains ~).
#' @examples
#' date_is_approximate(as_messydate(c("2012-06-02~", "2012-06-02")))
#' @export
date_is_approximate <- function(x) {
  stringi::stri_detect_regex(x, "\\~|\\%")
}

#' @describeIn operate_statements tests whether one or more messy dates are found
#'   before the common era, i.e. carry a negative (astronomical) year.
#'   Note that astronomical year zero (`"0000"`, historically 1 BCE) is not
#'   sign-negative and so returns `FALSE`; it is handled natively as a
#'   non-negative year throughout, which R's proleptic Gregorian `Date`
#'   supports directly.
#' @examples
#' date_is_bce(as_messydate(c("2012-06-02", "-2012-06-02")))
#' @export
date_is_bce <- function(x) {
  stringi::stri_detect_regex(x, "^-")
}


#' @describeIn operate_statements tests whether a messy date is the empty set,
#'   written `{}`.
#'   The empty set names no dates. It is the result, for example, of an
#'   intersection of two dates that do not overlap.
#'   ISO 8601-2:2019 requires a set to have at least one member,
#'   so `{}` is a messydates extension to the standard.
#'   A missing value (`NA`) is not the empty set: `NA` is a date that is not
#'   known, and `{}` is known to contain no date.
#' @examples
#' date_is_empty(as_messydate(c("{}", "2012-06-02", NA)))
#' @export
date_is_empty <- function(x) {
  out <- is_empty_notation(x)
  out[is.na(x)] <- NA
  out
}

# TRUE for each element written as the empty set, '{}'; FALSE for NA.
is_empty_notation <- function(x) {
  out <- stringi::stri_detect_regex(as.character(x), "^\\s*\\{\\s*\\}\\s*$")
  out[is.na(out)] <- FALSE
  out
}

# TRUE when any element of `x` is the empty set, '{}', or missing (NA).
.has_missing <- function(x) {
  any(is_empty_notation(x) | is.na(x))
}

# Applies an element-wise function `f` to `x` and gives '{}' or NA again where
# `x` was the empty set or missing. A stand-in date takes the place of each
# such element while `f` runs, so that `f` sees only dates and the recycling
# of other arguments does not change.
.with_missing <- function(x, f, ...) {
  empty <- is_empty_notation(x)
  miss <- is.na(x)
  y <- x
  y[empty | miss] <- "2000-01-01"
  out <- f(y, ...)
  out[empty] <- "{}"
  out[miss] <- NA
  out
}
