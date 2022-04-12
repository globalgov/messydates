#' Logical tests on messy dates
#'
#' These functions provide various logical tests for messy date objects.
#' `is_messydate()` tests whether the object inherits the `messydt` class.
#' If a more rigorous validation is required, see `validate_messydate()`.
#' `is_intersecting()` tests whether there is any intersection between
#' two messy dates, leveraging `intersect()`.
#' `is_element()` similarly tests whether a messy date can be found
#' within a messy date range or set.
#' `is_similar()` tests whether two dates contain similar components.
#' This can be useful for identifying dates that may be typos of one another.
#' `is_uncertain()` tests whether a date is uncertain.
#' Uncertain dates can contain markers that they are approximate (i.e. ~),
#' uncertain (i.e. % or ?), or be incomplete dates (i.e. year only).
#' @name logical
#' @param x,y Messy date or other class objects
#' @return A logical vector the length of the messy dates passed.
NULL
#> NULL

#' @rdname logical
#' @examples
#' is_messydate(as_messydate("2012-01-01"))
#' is_messydate(as.Date("2012-01-01"))
#' @export
is_messydate <- function(x) inherits(x, "messydt")

#' @rdname logical
#' @examples
#' is_intersecting(as_messydate("2012-01"),
#' as_messydate("2012-01-01..2012-02-22"))
#' is_intersecting(as_messydate("2012-01"),
#' as_messydate("2012-02-01..2012-02-22"))
#' @export
is_intersecting <- function(x, y) {
  length(intersect(x, y)) > 0
}

#' @rdname logical
#' @examples
#' is_element(as_messydate("2012-01-01"), as_messydate("2012-01"))
#' is_element(as_messydate("2012-01-01"), as_messydate("2012-02"))
#' @export
is_element <- function(x, y) {
  y <- as.character(expand(y)[[1]])
  is.element(x, y)
}

#' @rdname logical
#' @examples
#' is_similar(as_messydate("2012-06-02"), as_messydate("2012-02-06"))
#' is_similar(as_messydate("2012-06-22"), as_messydate("2012-02-06"))
#' @export
is_similar <- function(x, y) {
  year(x) == year(y) & month(x) == day(y) & day(x) == month(y)
}

#' @rdname logical
#' @examples
#' is_uncertain(as_messydate(c("2012-06-02", "2012-06")))
#' @export
is_uncertain <- function(x) {
  stringr::str_detect(x, "^[:digit:]{4}-[:digit:]{2}-[:digit:]{2}$|
                      |^-[:digit:]{4}-[:digit:]{2}-[:digit:]{2}$",
                      negate = TRUE)
}
