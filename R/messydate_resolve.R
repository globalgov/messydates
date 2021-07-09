#' Resolves for minimum values
#'
#' Gets minimum values from expanded date ranges,
#' unspecified dates or sets of dates.
#' @param ... a messydt object
#' @param na.rm Should NAs be removed? True by default.
#' @example
#' d <- "2014-01-01..2014-01-31"
#' min.messydt(as_messydate(d))
#' @export
min.messydt <- function(..., na.rm = TRUE){
  x <- list(...)
  y <- expand(x[[1]])
  y <- sapply(y, function(x) as.character(min(x, na.rm = na.rm)))
  y
}

#' Resolves for maximum values
#'
#' Gets maximum values from expanded date ranges,
#' unspecified dates or sets of dates.
#' @param ... a messydt object
#' @param na.rm Should NAs be removed? True by default.
#' @example
#' d <- "2014-01-01..2014-01-31"
#' max.messydt(as_messydate(d))
#' @export
max.messydt <- function(..., na.rm = TRUE){
  x <- list(...)
  y <- expand(x[[1]])
  y <- sapply(y, function(x) as.character(max(x, na.rm = na.rm)))
  y
}

#' Resolve for median values
#'
#' Gets median values from expanded date ranges,
#' unspecified dates or sets of dates.
#' @param ... a messydt object
#' @param na.rm Should NAs be removed? True by default.
#' @example
#' d <- "2014-01-01..2014-01-31"
#' median.messydt(as_messydate(d))
#' @export
median.messydt <- function(..., na.rm = TRUE){
  x <- list(...)
  y <- expand(x[[1]])
  y <- sapply(y, function(x) as.character(stats::median(x, na.rm = na.rm)))
  y
}

#' Resolve for mean values
#'
#' Gets mean values from expanded date ranges,
#' unspecified dates or sets of dates.
#' @param ... a messydt object
#' @param na.rm Should NAs be removed? True by default.
#' @example
#' d <- "2014-01-01..2014-01-31"
#' mean.messydt(as_messydate(d))
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

#' Resolve for modal values
#'
#' Gets modal values from expanded date ranges,
#' unspecified dates or sets of dates.
#' @param ... a messydt object
#' @param na.rm Should NAs be removed? True by default.
#' @example
#' d <- "2014-01-01..2014-01-31"
#' modal.messydt(as_messydate(d))
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
