#' Coercion from messy dates
#'
#' Coerces from messydt class to various date classes
#' @param x A `messydt` object
#' @param ... Arguments passed on to the S3 generics.
#' @param FUN A function that can be used to resolve expanded messy dates
#' into a single date.
#' For example, `min()`, `max()`, `mean()`, `median()`,
#' `modal()`, and `random()`.
#' @return A date object of `Date`, `POSIXct`, or `POSIXlt` class
#' @name from_messydate
NULL
#> NULL

#' @rdname from_messydate
#' @examples
#' as.Date(as_messydate("2012-01"), min)
#' as.Date(as_messydate("2012-01"), mean)
#' as.Date(as_messydate("2012-01"), max)
#' as.Date(as_messydate("2012-01"), median)
#' as.Date(as_messydate("2012-01"), modal)
#' as.Date(as_messydate("2012-01"), random)
#' @export
as.Date.messydt <- function(x, ..., FUN) {
  if (missing(FUN) & length(list(...)) > 0) FUN <- list(...)[[1]]
  x <- FUN(x)
  as.Date(x)
}

#' @rdname from_messydate
#' @export
as.POSIXct.messydt <- function(x, ..., FUN) {
  if (missing(FUN) & length(list(...)) > 0) FUN <- list(...)[[1]]
  x <- FUN(x)
  as.POSIXct(x)
}

#' @rdname from_messydate
#' @export
as.POSIXlt.messydt <- function(x, ..., FUN) {
  if (missing(FUN) & length(list(...)) > 0) FUN <- list(...)[[1]]
  x <- FUN(x)
  as.POSIXlt(x)
}
