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
#' @param ... a messydt object
#' @param na.rm Should NAs be removed? True by default.
#' @importFrom stringr str_detect
#' @return A single scalar or vector of dates
#' @examples
#' d <- as_messydate(c("2008-03-25", "?2012-02-27", "2001-01?", "2001~",
#' "2001-01-01..2001-02-02", "{2001-01-01,2001-02-02}",
#' "{2001-01,2001-02-02}", "2008-XX-31"))
#' d
#' min(d)
#' max(d)
#' mean(d)
#' median(d)
#' modal(d)
#' random(d)
#' @name resolve
NULL
#> NULL

#' @rdname resolve
#' @export
min.messydt <- function(..., na.rm = TRUE) {
  x <- list(...)
  y <- sapply(x, function(y) ifelse(is_uncertain(y), expand(y), y))
  y <- sapply(y, function(x) min(x, na.rm = na.rm))
  y
}

#' @rdname resolve
#' @export
max.messydt <- function(..., na.rm = TRUE) {
  x <- list(...)
  y <- sapply(x, function(y) ifelse(is_uncertain(y), expand(y), y))
  y <- sapply(y, function(x) max(x, na.rm = na.rm))
  y
}

#' @rdname resolve
#' @importFrom stats median
#' @export
median.messydt <- function(..., na.rm = TRUE) {
  x <- list(...)
  y <- sapply(x, function(y) ifelse(is_uncertain(y), expand(y), y))
  y <- sapply(y, function(z) {
    if (length(z) %% 2 == 0) {
      z <- unlist(z[-1])
      z <- as.character(median(z, na.rm = na.rm))
      z
    }
    else{
      z <- as.character(median(z, na.rm = na.rm))
      z
    }
  })
  y
}

#' @rdname resolve
#' @param trim the fraction (0 to 0.5) of observations to be trimmed
#' from each end of x before the mean is computed.
#' Values of trim outside that range are taken as the nearest endpoint.
#' @importFrom lubridate as_date
#' @export
mean.messydt <- function(..., trim = 0, na.rm = TRUE) {
  x <- list(...)
  y <- sapply(x, function(y) ifelse(is_uncertain(y), expand(y), y))
  y <- sapply(y, function(x) {
    if (length(x) > 1 & stringr::str_detect(x[1], "^-", negate = TRUE)) {
      x <- as.character(mean(as.Date(x), trim = 0, na.rm = TRUE))
    }
    if (length(x) > 1 & stringr::str_detect(x[1], "^-")) {
      x <- paste0("-", as.character(mean(lubridate::as_date(x),
                                         trim = 0, na.rm = TRUE)))
    }
    x
  })
  y
}

#' @rdname resolve
#' @export
modal <- function(..., na.rm = FALSE) UseMethod("modal")

#' @rdname resolve
#' @export
modal.messydt <- function(..., na.rm = TRUE) {
  x <- list(...)
  y <- sapply(x, function(y) ifelse(is_uncertain(y), expand(y), y))
  getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  y <- sapply(y, function(x) {
    if (length(x) > 1) x <- as.character(getmode(x))
    x
  })
  y
}

#' @rdname resolve
#' @param size a non-negative integer giving the number of items to choose.
#' @param replace should sampling be with replacement?
#' @param prob a vector of probability weights
#' for obtaining the elements of the vector being sampled.
#' @export
random <- function(..., size,
                   replace = FALSE,
                   prob = NULL) UseMethod("random")

#' @rdname resolve
#' @export
random.messydt <- function(...,
                           size,
                           replace = FALSE,
                           prob = NULL) {
  x <- list(...)
  y <- sapply(x, function(y) ifelse(is_uncertain(y), expand(y), y))
  y <- sapply(y, function(x) {
    if (length(x) > 1) x <- as.character(sample(x, size = 1))
    x
  })
  y
}
