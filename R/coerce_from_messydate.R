#' Coercion from messy dates
#' @description
#'   These functions coerce objects of `mdate` class to
#'   common date classes such as `Date`, `POSIXct`, and `POSIXlt`.
#'   Since `mdate` objects can hold multiple individual dates,
#'   however, an additional function must be passed as an argument
#'   so that these functions know how to coerce resolve multiple dates
#'   into a single date.
#'
#'   For example, one might wish to use the earliest possible date
#'   in any ranges of dates (`min`), the latest possible date (`max`),
#'   some notion of a central tendency (`mean`, `median`, or `modal`),
#'   or even a `random` selection from among the candidate dates.
#'
#'   These functions then, building on `expand()` and the resolve functions,
#'   are particularly useful in converting back out of the `mdate` class
#'   for use with existing methods and models,
#'   especially for checking the robustness of results.
#' @param x A `mdate` object
#' @param ... Arguments passed on to the S3 generics.
#' @param FUN A function that can be used to resolve expanded messy dates
#'   into a single date.
#'   For example, `min()`, `max()`, `mean()`, `median()`,
#'   `modal()`, and `random()`.
#' @return A date object of `Date`, `POSIXct`, or `POSIXlt` class
#' @name coerce_from
NULL

#' @rdname coerce_from
#' @examples
#' as.Date(as_messydate("2012-01"), FUN = vmin)
#' as.Date(as_messydate("2012-01-01"), FUN = vmean)
#' as.Date(as_messydate("2012-01"), FUN = vmax)
#' as.Date(as_messydate("2012-01"), FUN = vmedian)
#' as.Date(as_messydate("1000 BC"), FUN = vmax)
#' as.Date(as_messydate("1000 BC"), FUN = vmedian)
#' as.Date(as_messydate(c("-1000", "2020")), FUN = vmin)
#' @export
as.Date.mdate <- function(x, FUN = vmin, ...) {
  # # fix argument ordering issues
  # if (missing(FUN)){
  #   if(length(list(...)) > 0) FUN <- list(...)[[1]] else
  #     FUN <- messydates::min.mdate
  # }
  # if(missing(FUN)) FUN <- min
  x <- FUN(x)
  x <- suppressWarnings(ifelse(stringi::stri_detect_regex(x, "^-"),
                               lubridate::as_date(negative_dates(x)),
                               lubridate::as_date(zero_padding(x))))
  as.Date(x, origin = "1970-01-01")
}

#' @rdname coerce_from
#' @export
as.POSIXct.mdate <- function(x, tz = "UTC", FUN = vmin, ...) {
  # if (missing(FUN) & length(list(...)) > 0) FUN <- list(...)[[1]]
  x <- FUN(x)
  if (stringi::stri_detect_regex(x, "^-")) {
    stop("For conversion of negative dates from mdate class use as.Date()")
  }
  as.POSIXct(as.character(x), tz = tz)
}

#' @rdname coerce_from
#' @export
as.POSIXlt.mdate <- function(x, tz = "UTC", FUN = vmin, ...) {
  # if (missing(FUN) & length(list(...)) > 0) FUN <- list(...)[[1]]
  x <- FUN(x)
  if (stringi::stri_detect_regex(x, "^-")) {
    stop("For conversion of negative dates from mdate class use as.Date()")
  }
  as.POSIXlt(as.character(x), tz = tz)
}

# Helper function for returning negative dates in date formats
#' @importFrom stringi stri_replace_all_regex stri_extract_all_regex
#' @importFrom lubridate ymd years as_date
negative_dates <- function(x) {
  x <- stringi::stri_replace_all_regex(x, "^-", "")
  y <- stringi::stri_extract_all_regex(x, "^[0-9]{4}")
  md <- stringi::stri_replace_all_regex(x, "^[0-9]{4}", "0000")
  x <- lubridate::ymd(md) - lubridate::years(y)
  x <- lubridate::as_date(x)
  x
}

#' @export
as.data.frame.mdate <- function(x, ...) {
  as.data.frame.vector(x, ...)
}

#' @export
as.list.mdate <- function(x, ...) {
  lapply(unclass(x), as_messydate)
}

#' @export
as.double.mdate <- function(x, ...) {
  if(any(is_bce(x))) x[is_bce(x)] <- negative_dates(x)[is_bce(x)]
  as.double(lubridate::as_date(x))
}

