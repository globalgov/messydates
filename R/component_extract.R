#' Extracting components from messy dates
#' @description
#'   These functions allow the extraction of particular date components
#'   from messy dates, such as the `year()`, `month()`, and `day()`.
#'   `precision()` allows for the identification of the greatest level of
#'   precision in (currently) the first element of each date.
#' @param x A `mdate` object
#' @return `year()`, `month()`, and `day()` extraction return the integer
#'   for the requested date component.
#'   `precision()` returns the level of greatest precision for each date.
#' @name component_extract
NULL
#> NULL

#' @rdname component_extract
#' @examples
#' year(as_messydate(c("2012-02-03","2012","2012-02")))
#' @export
year <- function(x) {
  x <- stringi::stri_replace_all_regex(x, "\\.\\..+", "")
  x <- stringi::stri_replace_all_regex(x, "-.+", "")
  as.integer(x)
}

#' @rdname component_extract
#' @examples
#' month(as_messydate(c("2012-02-03","2012","2012-02")))
#' @export
month <- function(x) {
  x <- sapply(x, function(y) {
    stringi::stri_split_regex(y, "-")[[1]][2]
  })
  as.integer(x)
}

#' @rdname component_extract
#' @examples
#' day(as_messydate(c("2012-02-03","2012","2012-02")))
#' @export
day <- function(x) {
  x <- sapply(x, function(y) {
    stringi::stri_split_regex(y, "-")[[1]][3]
  })
  as.integer(x)
}

#' @rdname component_extract
#' @export
precision <- function(x) UseMethod("precision")

#' @rdname component_extract
#' @section Precision:
#'   Date precision is measured relative to the day in \eqn{1/days(x)}.
#'   That is, a date measured to the day will return a precision score
#'   of 1, a date measured to the month will return a precision score of
#'   between \eqn{1/28} and \eqn{1/31}, and annual measures will have
#'   a precision of between \eqn{1/365} and \eqn{1/366}.
#' @examples
#' precision(as_messydate(c("2012-02-03","2012","2012-02")))
#' @export
precision.mdate <- function(x) {
  out <- expand(x)
  1/lengths(out)
}
