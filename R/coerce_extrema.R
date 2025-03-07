#' Resolves messy dates into an extrema
#' @description
#'   This collection of S3 methods 'resolve' messy dates into a single date
#'   according to some explicit bias,
#'   such as returning the minimum or maximum date,
#'   the mean, median, or modal date,
#'   or a random date from among the possible resolutions for each messy date.
#'   If the date is not 'messy' (i.e. has no annotations)
#'   then just that precise date is returned.
#'   This can be useful for various descriptive or inferential projects.
#' @param ... a mdate object
#' @param na.rm Should NAs be removed? True by default.
#' @importFrom stringi stri_detect_regex stri_replace_all_regex
#' @return A single scalar or vector of dates
#' @examples
#' d <- as_messydate(c("2008-03-25", "?2012-02-27", "2001-01?", "2001~",
#'   "2001-01-01..2001-02-02", "{2001-01-01,2001-02-02}",
#'   "{2001-01,2001-02-02}", "2008-XX-31", "-0050-01-01"))
#' d
#' @name coerce_extrema
NULL

#' @rdname coerce_extrema
#' @export
vmin <- function(..., na.rm = FALSE) UseMethod("vmin")

#' @rdname coerce_extrema
#' @examples
#' vmin(d)
#' @export
vmin.mdate <- function(..., na.rm = TRUE){
  d <- list(...)[[1]]
  dates <- d
  if(na.rm) dates <- stats::na.omit(d)
  dates <- stringi::stri_replace_all_regex(dates, "~|\\?", "")
  dates <- .remove_post(dates)
  dates <- .replace_earliest(dates)
  mdate(dates)
}

#' @rdname coerce_extrema
#' @examples
#' min(d)
#' @export
min.mdate <- function(..., na.rm = TRUE){
  d <- list(...)[[1]]
  dates <- d
  if(na.rm) dates <- stats::na.omit(d)
  dates <- stringi::stri_replace_all_regex(dates, "~|\\?", "")
  dates <- .remove_post(dates)
  dates <- .replace_earliest(dates)
  dates <- mdate(dates)
  if(any(is_bce(dates)))
    dates[is_bce(dates)][order(as.character(dates[is_bce(dates)]),
                               decreasing = TRUE)][1] else
                                 dates[order(as.character(dates))==1]
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

#' @rdname coerce_extrema
#' @export
vmax <- function(..., na.rm = FALSE) UseMethod("vmax")

#' @rdname coerce_extrema
#' @examples
#' vmax(d)
#' @export
vmax.mdate <- function(..., na.rm = TRUE){
  d <- list(...)[[1]]
  dates <- d
  if(na.rm) dates <- stats::na.omit(d)
  dates <- stringi::stri_replace_all_regex(dates, "~|\\?", "")
  dates <- unspecified_months(dates)
  dates <- .remove_pre(dates)
  dates <- .replace_latest(dates)
  mdate(dates)
}

#' @rdname coerce_extrema
#' @examples
#' max(d)
#' @export
max.mdate <- function(..., na.rm = TRUE) {

  d <- list(...)[[1]]
  dates <- stringi::stri_replace_all_regex(d, "~|\\?", "")
  dates <- unspecified_months(dates)
  dates <- .remove_pre(dates)
  dates <- .replace_latest(dates)
  dates <- mdate(dates)
  if(all(is_bce(dates), na.rm = TRUE))
    dates[order(dates, decreasing = TRUE)][1] else
      dates[!is_bce(dates)][order(as.character(dates[!is_bce(dates)]),
                                  decreasing = TRUE)][1]

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

