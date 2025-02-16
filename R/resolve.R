#' Resolves messy dates into a single value
#'
#' This collection of S3 methods 'resolve' messy dates into a single date
#' according to some explicit bias,
#' such as returning the minimum or maximum date,
#' the mean, median, or modal date,
#' or a random date from among the possible resolutions for each messy date.
#' If the date is not 'messy' (i.e. has no annotations)
#' then just that precise date is returned.
#' This can be useful for various descriptive or inferential projects.
#' @param ... a mdate object
#' @param na.rm Should NAs be removed? True by default.
#' @importFrom stringr str_detect
#' @return A single scalar or vector of dates
#' @examples
#' d <- as_messydate(c("2008-03-25", "?2012-02-27", "2001-01?", "2001~",
#' "2001-01-01..2001-02-02", "{2001-01-01,2001-02-02}",
#' "{2001-01,2001-02-02}", "2008-XX-31"))
#' d
#' max(d)
#' mean(d)
#' median(d)
#' modal(d)
#' @name resolve
NULL
#> NULL

#' @rdname resolve
#' @param recursive If recursive = TRUE, then the dates will be resolved
#'   to a single date. If recursive = FALSE, then the dates will be resolved
#'   to a vector the length of the original vector.
#'   By default FALSE.
#' @examples
#' min(d)
#' @export
min.mdate <- function(..., na.rm = TRUE, recursive = FALSE){
  d <- list(...)[[1]]
  dates <- stringi::stri_replace_all_regex(d, "~|\\?", "")
  dates <- .remove_post(dates)
  dates <- .replace_earliest(dates)
  # if(any(stringi::stri_detect_regex(dates, "^~")))
  #   dates <- expand_approximate_years(dates, approx_range = approx_range)
  # if(any(stringi::stri_detect_regex(dates, "[:digit:]~"))){
  #   dates <- expand_approximate_months(dates, approx_range = approx_range)
  #   dates <- expand_approximate_days(dates, approx_range = approx_range)
  # }
  if(recursive){
    if(any(is_bce(dates))) max(dates[is_bce(dates)]) else
      min(dates)
  } else dates
}

.remove_post <- function(dates){
  dates <- stringi::stri_replace_all_regex(dates, "\\.\\.$|,.*$|\\{", "")
  dates <- stringi::stri_replace_all_regex(dates, "^(.+)\\.\\..*$", "$1")
  dates <- stringi::stri_replace_all_regex(dates, "\\.\\.", "")
  dates
}

.replace_earliest <- function(dates){
  dates <- stringi::stri_replace_last_regex(dates,
                                            "XX", "01")
  dates <- stringi::stri_replace_last_regex(dates,
                                            "^(.*[:digit:]{4})$", "$1-01-01")
  dates <- stringi::stri_replace_last_regex(dates,
                                   "^(.*[:digit:]{4})-([:digit:]{2})$", "$1-$2-01")
  # dates <- stringi::stri_replace_last_regex(dates,
  #                                           "^-([:digit:]{4})-([:digit:]{2})$", "-$1-$2-01")
  dates
}

#' @rdname resolve
#' @export
max.mdate <- function(..., na.rm = TRUE, recursive = FALSE) {

  d <- list(...)[[1]]
  dates <- stringi::stri_replace_all_regex(d, "~|\\?", "")
  dates <- unspecified_months(dates)
  dates <- .remove_pre(dates)
  dates <- .replace_latest(dates)

  if(recursive){
    if(any(is_bce(dates))) max(dates[!is_bce(dates)]) else
      max(dates)
  } else dates

}

.remove_pre <- function(dates){
  dates <- stringi::stri_replace_all_regex(dates, "^\\.\\.|^.*,|\\}", "")
  dates <- stringi::stri_replace_all_regex(dates, "^.*\\.\\.(.+)$", "$1")
  dates <- stringi::stri_replace_all_regex(dates, "\\.\\.", "")
  dates
}

.replace_latest <- function(dates){
  dates <- stringi::stri_replace_last_regex(dates,
                                            "^(.*[:digit:]{4})$", "$1-12-31")
  dates <- stringi::stri_replace_last_regex(dates,
                                            "-XX-", "-12-")
  dates
}

#' @rdname resolve
#' @importFrom stats median
#' @export
median.mdate <- function(..., na.rm = TRUE, recursive = FALSE) {
  x <- as.list(...)
  y <- unlist(lapply(x, function(y) ifelse(!is_precise(y), expand(y), y)),
              recursive = recursive)
  if(recursive){
    if (length(y) %% 2 == 0) {
      as.character(median(unlist(y[-1]), na.rm = na.rm))
    }
    else{
      as.character(median(y, na.rm = na.rm))
    }
  } else {
    unlist(lapply(y, function(z) {
      if (length(z) %% 2 == 0) {
        z <- unlist(z[-1])
        z <- as.character(median(z, na.rm = na.rm))
        z
      }
      else{
        z <- as.character(median(z, na.rm = na.rm))
        z
      }
    }), recursive = FALSE)
  }
}

#' @rdname resolve
#' @param trim the fraction (0 to 0.5) of observations to be trimmed
#' from each end of x before the mean is computed.
#' Values of trim outside that range are taken as the nearest endpoint.
#' @importFrom lubridate as_date
#' @export
mean.mdate <- function(..., trim = 0, na.rm = TRUE, recursive = FALSE) {
  x <- as.list(...)
  y <- unlist(lapply(x, function(y) ifelse(!is_precise(y), expand(y), y)),
              recursive = recursive)
  if(recursive){
    if (length(y) > 1 & stringr::str_detect(y[1], "^-", negate = TRUE)) {
      y <- as.character(mean(as.Date(y), trim = 0, na.rm = TRUE))
    }
    if (length(y) > 1 & stringr::str_detect(y[1], "^-")) {
      y <- paste0("-", as.character(mean(lubridate::as_date(y),
                                         trim = 0, na.rm = TRUE)))
      y <- zero_padding(y)
    }
    y
  } else {
    unlist(lapply(y, function(x) {
      if (length(x) > 1 & stringr::str_detect(x[1], "^-", negate = TRUE)) {
        x <- as.character(mean(as.Date(x), trim = 0, na.rm = TRUE))
      }
      if (length(x) > 1 & stringr::str_detect(x[1], "^-")) {
        x <- paste0("-", as.character(mean(lubridate::as_date(x),
                                           trim = 0, na.rm = TRUE)))
        x <- zero_padding(x)
      }
      x
    }), recursive = FALSE)
  }
}

#' @rdname resolve
#' @export
modal <- function(..., na.rm = FALSE, recursive = FALSE) UseMethod("modal")

#' @rdname resolve
#' @export
modal.mdate <- function(..., na.rm = TRUE, recursive = FALSE) {
  x <- as.list(...)
  y <- unlist(lapply(x, function(y) ifelse(!is_precise(y), expand(y), y)),
              recursive = recursive)
  getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  if(recursive){
    as.character(getmode(y))
  } else {
    unlist(lapply(y, function(x) {
      if (length(x) > 1) x <- as.character(getmode(x))
      x
    }), recursive = FALSE)
  }
}

#' @rdname resolve
#' @param size a non-negative integer giving the number of items to choose.
#' @param replace should sampling be with replacement?
#' @param prob a vector of probability weights
#' for obtaining the elements of the vector being sampled.
#' @export
random <- function(..., size,
                   replace = FALSE,
                   prob = NULL, recursive = FALSE) UseMethod("random")

#' @rdname resolve
#' @examples
#' random(d)
#' @export
random.mdate <- function(...,
                           size,
                           replace = FALSE,
                           prob = NULL, recursive = FALSE) {
  x <- as.list(...)
  y <- unlist(lapply(x, function(y) ifelse(!is_precise(y), expand(y), y)),
              recursive = recursive)
  if(recursive){
    as.character(sample(y, size = 1))
  } else {
    unlist(lapply(y, function(x) {
      if (length(x) > 1) x <- as.character(sample(x, size = 1))
      x
    }), recursive = FALSE)
  }
}
