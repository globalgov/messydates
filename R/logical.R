#' Logical tests on messy dates
#'
#' These functions provide various logical tests for messy date objects.
#' @name logical
#' @param x,y `mdate` or other class objects
#' @return A logical vector the same length as the `mdate` passed.
NULL

#' Logical tests on messy dates
#'
#' These functions provide various logical tests for messy date objects.
#' @param x,y `mdate` or other class objects
#' @return A logical vector the same length as the `mdate` passed.
#' @describeIn logical tests whether the object inherits the `mdate` class.
#'   If more rigorous validation is required, see `validate_messydate()`.
#' @examples
#' is_messydate(as_messydate("2012-01-01"))
#' is_messydate(as.Date("2012-01-01"))
#' @export
is_messydate <- function(x) {
  inherits(x, "mdate")
}

#' @describeIn logical tests whether there is any intersection between
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

#' @describeIn logical tests whether a messy date can be found
#'   within a messy date range or set.
#' @examples
#' is_element(as_messydate("2012-01-01"), as_messydate("2012-01"))
#' is_element(as_messydate("2012-01-01"), as_messydate("2012-02"))
#' @export
is_element <- function(x, y) {
  y <- as.character(expand(y)[[1]])
  is.element(x, y)
}

#' @describeIn logical tests whether two dates contain similar components.
#'   This can be useful for identifying dates that may be typos of one another.
#' @examples
#' is_similar(as_messydate("2012-06-02"), as_messydate("2012-02-06"))
#' is_similar(as_messydate("2012-06-22"), as_messydate("2012-02-06"))
#' @export
is_similar <- function(x, y) {
  year(x) == year(y) & month(x) == day(y) & day(x) == month(y)
}

#' @describeIn logical tests whether a date is precise (i.e. an 8 digit date).
#'   Non-precise dates contain markers that they are approximate (i.e. ~),
#'   unreliable (i.e. ?), are incomplete dates (i.e. year only),
#'   or date ranges and sets.
#' @examples
#' is_precise(as_messydate(c("2012-06-02", "2012-06")))
#' @export
is_precise <- function(x) {
  stringr::str_detect(x, "^[:digit:]{4}-[:digit:]{2}-[:digit:]{2}$|
                      |^-[:digit:]{4}-[:digit:]{2}-[:digit:]{2}$")
}

#' @describeIn logical tests whether a date is uncertain (i.e. contains ?).
#' @examples
#' is_uncertain(as_messydate(c("2012-06-02", "2012-06-02?")))
#' @export
is_uncertain <- function(x) {
  stringr::str_detect(x, "\\?")
}

#' @describeIn logical tests whether a date is approximate (i.e. contains ~).
#' @examples
#' is_approximate(as_messydate(c("2012-06-02~", "2012-06-02")))
#' @export
is_approximate <- function(x) {
  stringr::str_detect(x, "\\~")
}
