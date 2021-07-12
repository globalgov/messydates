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
#' @examples
#' d <- as_messydate("2014-01-01..2014-01-31")
#' d
#' min(d)
#' max(d)
#' mean(d)
#' median(d)
#' modal(d)
NULL
#> NULL

#' @rdname resolve
#' @export
min.messydt <- function(..., na.rm = TRUE){
  x <- list(...)
  y <- expand(x[[1]])
  y <- sapply(y, function(x) as.character(min(x, na.rm = na.rm)))
  y
}

#' @rdname resolve
#' @export
max.messydt <- function(..., na.rm = TRUE){
  x <- list(...)
  y <- expand(x[[1]])
  y <- sapply(y, function(x) as.character(max(x, na.rm = na.rm)))
  y
}

#' @rdname resolve
#' @export
median.messydt <- function(..., na.rm = TRUE){
  x <- list(...)
  y <- expand(x[[1]])
  y <- sapply(y, function(x) as.character(stats::median(x, na.rm = na.rm)))
  y
}

#' @rdname resolve
#' @export
mean.messydt <- function(..., na.rm = TRUE){
  x <- list(...)
  y <- expand(x[[1]])
  y <- sapply(y, function(x){
    if(length(x)>1) x <- as.character(mean(x, na.rm = na.rm))
    x
  })
  y
}

#' @export
modal <- function(..., na.rm = FALSE) UseMethod("modal")

#' @rdname resolve
#' @export
modal.messydt <- function(..., na.rm = TRUE){
  x <- list(...)
  y <- expand(x[[1]])
  getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  y <- sapply(y, function(x){
    if(length(x)>1) x <- as.character(getmode(x))
    x
  })
  y
}
