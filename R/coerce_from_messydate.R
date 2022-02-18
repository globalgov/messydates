#' Coercion from messy dates
#'
#' @description
#' These functions coerce objects of `messydt` class to
#' common date classes such as `Date`, `POSIXct`, and `POSIXlt`.
#' Since `messydt` objects can hold multiple individual dates,
#' however, an additional function must be passed as an argument
#' so that these functions know how to coerce resolve multiple dates
#' into a single date.
#'
#' For example, one might wish to use the earliest possible date
#' in any ranges of dates (`min`), the latest possible date (`max`),
#' some notion of a central tendency (`mean`, `median`, or `modal`),
#' or even a `random` selection from amongst the candidate dates.
#'
#' These functions then, building on `expand()` and the resolve functions,
#' are particularly useful in converting back out of the `messydt` class
#' for use with existing methods and models,
#' especially for checking the robustness of results.
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
#' as.Date(as_messydate("2012-01-01"), mean)
#' as.Date(as_messydate("2012-01"), max)
#' as.Date(as_messydate("2012-01"), median)
#' as.Date(as_messydate("2012-01"), modal)
#' as.Date(as_messydate("2012-01"), random)
#' as.Date(as_messydate("1000 BC"), max)
#' as.Date(as_messydate("1000 BC"), mean)
#' as.Date(as_messydate("1000 BC"), median)
#' as.Date(as_messydate(c("-1000", "2020")), min)
#' @export
as.Date.messydt <- function(x, ..., FUN) {
  if (missing(FUN) & length(list(...)) > 0) FUN <- list(...)[[1]]
  x <- FUN(x)
  x <- ifelse(stringr::str_detect(x, "^-"),
              lubridate::as_date(negative_dates(x)),
              lubridate::as_date(x))
  as.Date(x, origin = "1970-01-01")
}

#' @rdname from_messydate
#' @export
as.POSIXct.messydt <- function(x, ..., FUN) {
  if (missing(FUN) & length(list(...)) > 0) FUN <- list(...)[[1]]
  x <- FUN(x)
  if (stringr::str_detect(x, "^-")) {
    stop("For conversion of negative dates from messydt class use as.Date()")
  }
  as.POSIXct(x)
}

#' @rdname from_messydate
#' @export
as.POSIXlt.messydt <- function(x, ..., FUN) {
  if (missing(FUN) & length(list(...)) > 0) FUN <- list(...)[[1]]
  x <- FUN(x)
  if (stringr::str_detect(x, "^-")) {
    stop("For conversion of negative dates from messydt class use as.Date()")
  }
  as.POSIXlt(x)
}

#' Helper function for returning negative dates in date formats
#' @rdname from_messydate
#' @importFrom stringr str_remove str_replace
#' @importFrom lubridate ymd years as_date
negative_dates <- function(x) {
  x <- ifelse(stringr::str_detect(x, "^-[0-9]{3}-"),
              stringr::str_replace(x, "^-", "0"),
              stringr::str_remove(x, "^-"))
  y <- stringr::str_extract(x, "^[0-9]{4}")
  md <- stringr::str_replace(x, "^[0-9]{4}", "0000")
  x <- lubridate::ymd(md) - lubridate::years(y)
  x <- lubridate::as_date(x)
  x
}
