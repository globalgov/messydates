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
#' @export
as.Date.messydt <- function(x,
                            resolve = c("min","max","median","mean")){

  resolve <- match.arg(resolve)

  if(resolve == "min") x <- min(x)
  if(resolve == "max") x <- max(x)
  if(resolve == "mean") x <- mean(x)
  if(resolve == "median") x <- stats::median(x)

  as.Date(x)
}

#' @rdname from_messydate
#' @export
as.POSIXct.messydt <- function(x,
                               resolve = c("min","max","median","mean")){

  resolve <- match.arg(resolve)

  if(resolve == "min") x <- min(x)
  if(resolve == "max") x <- max(x)
  if(resolve == "mean") x <- mean(x)
  if(resolve == "median") x <- stats::median(x)

  as.POSIXct(x)
}

#' @rdname from_messydate
#' @export
as.POSIXlt.messydt <- function(x,
                               resolve = c("min","max","median","mean")){

  resolve <- match.arg(resolve)

  if(resolve == "min") x <- min(x)
  if(resolve == "max") x <- max(x)
  if(resolve == "mean") x <- mean(x)
  if(resolve == "median") x <- stats::median(x)

  as.POSIXlt(x)
}
