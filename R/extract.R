#' Extracting components from messy dates
#'
#' These functions allow the extraction of particular date components
#' from messy dates, such as the `year()`, `month()`, and `day()`.
#' `precision()` allows for the identification of the greatest level of
#' precision in (currently) the first element of each date.
#' @param x A `messydt` object
#' @return `year()`, `month()`, and `day()` extraction return the integer
#' for the requested date component.
#' `precision()` returns the level of greatest precision for each date.
#' @name extract
NULL
#> NULL

#' @rdname extract
#' @examples
#' year(as_messydate(c("2012-02-03","2012","2012-02")))
#' @export
year <- function(x) {
  x <- sapply(x, function(y) {
    stringr::str_split(y, "-")[[1]][1]
  })
  as.integer(x)
}

#' @rdname extract
#' @examples
#' month(as_messydate(c("2012-02-03","2012","2012-02")))
#' @export
month <- function(x) {
  x <- sapply(x, function(y) {
    stringr::str_split(y, "-")[[1]][2]
  })
  as.integer(x)
}

#' @rdname extract
#' @examples
#' day(as_messydate(c("2012-02-03","2012","2012-02")))
#' @export
day <- function(x) {
  x <- sapply(x, function(y) {
    stringr::str_split(y, "-")[[1]][3]
  })
  as.integer(x)
}

#' @rdname extract
#' @importFrom stringr str_remove_all str_count
#' @examples
#' precision(as_messydate(c("2012-02-03","2012","2012-02")))
#' @export
precision <- function(x) {
  x <- expand(x)
  out <- sum(lengths(x))
  out
}
