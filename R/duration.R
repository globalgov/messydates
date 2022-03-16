#' Get the duration of a date interval
#'
#' @param x A messydt object
#' @param y A messydt object
#' @param resolve How do you want to resolve expanded messy dates
#' into a single date?
#' Options are: "min", "max", "mean", "median", "modal", and "random".
#' @return The numerical difference of dates in days
#' @examples
#' md_duration(as_messydate("2001"), as_messydate("2003"), "min")
#' md_duration(as_messydate("2001"), as_messydate("2003"), "max")
#' md_duration(as_messydate("2001"), as_messydate("2003"), "mean")
#' md_duration(as_messydate("2001"), as_messydate("2003"), "median")
#' md_duration(as_messydate("2001"), as_messydate("2003"), "modal")
#' md_duration(as_messydate("2001"), as_messydate("2003"), "random")
#' @export
md_duration <- function(x, y, resolve) {
  if (resolve == "min") {
    x <- as.Date(x, min)
    y <- as.Date(y, min)
  }
  if (resolve == "mean") {
    x <- as.Date(x, mean)
    y <- as.Date(y, mean)
  }
  if (resolve == "max") {
    x <- as.Date(x, max)
    y <- as.Date(y, max)
  }
  if (resolve == "modal") {
    x <- as.Date(x, modal)
    y <- as.Date(y, modal)
  }
  if (resolve == "median") {
    x <- as.Date(x, median)
    y <- as.Date(y, median)
  }
  if (resolve == "random") {
    x <- as.Date(x, random)
    y <- as.Date(y, random)
  }
  out <- as.numeric(lubridate::as_date(y)) - as.numeric(lubridate::as_date(x))
  out
}
