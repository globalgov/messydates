#' Coercion from messy dates
#'
#' Coerces from messydt class to date class
#' @param x a messydt object
#' @param resolve How do you want the date ranges to be resolved?
#' Before coercing dates, one should chose how to resolve ranges
#' so that dates can be coerced to other date classes.
#' Choices are "min","max","median", or "mean".
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
as.Date.messydt <- function(x, FUN){
  x <- FUN(x)
  as.Date(x)
}

#' @rdname from_messydate
#' @export
as.POSIXct.messydt <- function(x, FUN){
  x <- FUN(x)
  as.POSIXct(x)
}

#' @rdname from_messydate
#' @export
as.POSIXlt.messydt <- function(x, FUN){
  x <- FUN(x)
  as.POSIXlt(x)
}
