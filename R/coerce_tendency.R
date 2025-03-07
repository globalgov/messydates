#' Resolves messy dates into a central tendency
#' @description
#'   These functions resolve messydates by their central tendency.
#'   While the functions `mean()`, `median()`, and `modal()` summarise the
#'   vector to a single value, `v*()` versions return a vector of the same length.
#' @name coerce_tendency
#' @inheritParams coerce_extrema
#' @examples
#' d <- as_messydate(c("2008-03-25", "?2012-02-27", "2001-01?", "2001~",
#'   "2001-01-01..2001-02-02", "{2001-01-01,2001-02-02}",
#'   "{2001-01,2001-02-02}", "2008-XX-31", "-0050-01-01"))
#' d
NULL

#' @rdname coerce_tendency
#' @importFrom stats median
#' @examples
#' median(d)
#' @export
median.mdate <- function(..., na.rm = TRUE) {

  x <- list(...)[[1]]
  y <- unlist(expand(x))
  y <- .order_messy(y)
  median(y, na.rm = na.rm)
}

.order_messy <- function(y){
  if(any(is_bce(y))){
    bcey <- y[is_bce(y)]
    cey <- y[!is_bce(y)]
    c(bcey[order(bcey, decreasing = TRUE)],
           cey[order(cey)])
  } else {
    y[order(y)]
  }
}

#' @rdname coerce_tendency
#' @export
vmedian <- function(..., na.rm = TRUE) UseMethod("vmedian")

#' @rdname coerce_tendency
#' @importFrom stats median
#' @examples
#' vmedian(d)
#' @export
vmedian.mdate <- function(..., na.rm = TRUE) {

  x <- as.list(...)
  vapply(x, function(y){
    z <- suppressWarnings(median(y, na.rm = na.rm))
    if(is.na(z)){
      if(length(expand(y)[[1]]) %% 2 == 0)
        z <- median(.order_messy(expand(y)[[1]])[-1])
    }
    z
  }, FUN.VALUE = character(1))
}

#' @rdname coerce_tendency
#' @param trim the fraction (0 to 0.5) of observations to be trimmed
#'   from each end of x before the mean is computed.
#'   Values of trim outside that range are taken as the nearest endpoint.
#' @importFrom lubridate as_date
#' @examples
#' mean(d)
#' @export
mean.mdate <- function(..., trim = 0, na.rm = TRUE) {

  x <- list(...)[[1]]
  y <- unlist(expand(x))
  as.character(lubridate::as_date(mean(as.double(lubridate::as_date(y)))))
}


#' @rdname coerce_tendency
#' @export
vmean <- function(..., na.rm = TRUE) UseMethod("vmean")

#' @rdname coerce_tendency
#' @examples
#' vmean(d)
#' @export
vmean.mdate <- function(..., trim = 0, na.rm = TRUE) {
  x <- list(...)[[1]]
  vapply(expand(x), function(y)
    as.character(lubridate::as_date(mean(as.double(lubridate::as_date(y))))),
         FUN.VALUE = character(1))
}

