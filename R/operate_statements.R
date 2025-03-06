# Statements ####

#' Logical statements on messy dates
#' @description
#'   These functions provide various logical statements about messy date objects.
#' @name operate_statements
#' @param x,y,e1,e2 `mdate` or other class objects
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
#' is_similar(as_messydate("2012-06-02"), as_messydate("2012-02-06"))
#' is_similar(as_messydate("2012-06-22"), as_messydate("2012-02-06"))
#' @export
is_similar <- function(x, y) {
  year(x) == year(y) & month(x) == day(y) & day(x) == month(y)
}

#' @describeIn operate_statements tests whether a date is precise (i.e. an 8 digit date).
#'   Non-precise dates contain markers that they are approximate (i.e. ~),
#'   unreliable (i.e. ?), are incomplete dates (i.e. year only),
#'   or date ranges and sets.
#' @examples
#' is_precise(as_messydate(c("2012-06-02", "2012-06")))
#' @export
is_precise <- function(x) {
  stringi::stri_detect_regex(x, "^[:digit:]{4}-[:digit:]{2}-[:digit:]{2}$|
                      |^-[:digit:]{4}-[:digit:]{2}-[:digit:]{2}$")
}

#' @describeIn operate_statements tests whether a date is uncertain (i.e. contains ?).
#' @examples
#' is_uncertain(as_messydate(c("2012-06-02", "2012-06-02?")))
#' @export
is_uncertain <- function(x) {
  stringi::stri_detect_regex(x, "\\?|\\%")
}

#' @describeIn operate_statements tests whether a date is approximate (i.e. contains ~).
#' @examples
#' is_approximate(as_messydate(c("2012-06-02~", "2012-06-02")))
#' @export
is_approximate <- function(x) {
  stringi::stri_detect_regex(x, "\\~|\\%")
}

#' @describeIn operate_statements tests whether one or more messy dates are found
#'   before the common era.
#' @examples
#' is_bce(as_messydate(c("2012-06-02", "-2012-06-02")))
#' @export
is_bce <- function(x) {
  stringi::stri_detect_regex(x, "^-")
}

