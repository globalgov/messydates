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
#' @return The date values resolved according to min,
#' max, mean, median, modal or random
#' @examples
#' d <- as_messydate("2014-01-01..2014-01-31")
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
#' @return The minimum date values
#' @export
min.messydt <- function(..., na.rm = TRUE) {
  x <- list(...)
  y <- expand(x[[1]])
  y <- sapply(y, function(x) as.character(min(x, na.rm = na.rm)))
  y
}

#' @rdname resolve
#' @return The maximum date values
#' @export
max.messydt <- function(..., na.rm = TRUE) {
  x <- list(...)
  y <- expand(x[[1]])
  y <- sapply(y, function(x) as.character(max(x, na.rm = na.rm)))
  y
}

#' @rdname resolve
#' @return The median date values
#' @importFrom stats median
#' @export
median.messydt <- function(..., na.rm = TRUE) {
  x <- list(...)
  y <- expand(x[[1]])
  y <- sapply(y, function(x) as.character(stats::median(x, na.rm = na.rm)))
  y
}

#' @rdname resolve
#' @return The mean date values
#' @param trim the fraction (0 to 0.5) of observations to be trimmed
#' from each end of x before the mean is computed.
#' Values of trim outside that range are taken as the nearest endpoint.
#' @export
mean.messydt <- function(..., trim = 0, na.rm = TRUE) {
  x <- list(...)
  y <- expand(x[[1]])
  y <- sapply(y, function(x) {
    if (length(x) > 1) x <- as.character(mean(as.Date(x),
                                              trim = 0, na.rm = TRUE))
    x
  })
  y
}

#' @rdname resolve
#' @return The modal date values
#' @export
modal <- function(..., na.rm = FALSE) UseMethod("modal")

#' @rdname resolve
#' @return The modal date values
#' @export
modal.messydt <- function(..., na.rm = TRUE) {
  x <- list(...)
  y <- expand(x[[1]])
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
#' @return A random date value from the range of dates
#' @export
random <- function(..., size,
                   replace = FALSE,
                   prob = NULL) UseMethod("random")

#' @rdname resolve
#' @return A random date value from the range of dates
#' @export
random.messydt <- function(...,
                           size,
                           replace = FALSE,
                           prob = NULL) {
  x <- list(...)
  y <- expand(x[[1]])
  y <- sapply(y, function(x) {
    if (length(x) > 1) x <- as.character(sample(x, size = 1))
    x
  })
  y
}
