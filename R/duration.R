#' A duration class for mdates
#' @description
#'   The `mdates_duration` class introduces methods that annotate a duration or
#'   period with representations of its uncertainty.
#' @details
#'   Most R packages handle duration and periods as exact time or date intervals.
#'   However, this is not possible for 'messy' dates where uncertainty or
#'   approximation might be present.
#'   The `mdates_duration` class accounts for uncertainty and approximation
#'   in `mdate` objects to return their duration as a range of possible dates.
#' @param x An `mdate` variable with ranges.
#' @param approx_range Range to expand approximate dates, in days.
#'   If 3, for example, adds 3 days; if -3, removes 3 days from both sides.
#' @return Object of class `description`
#' @name duration_class
#' @examples
#' messyduration(as_messydate(c("2010-01-01..2010-12-31", "2010-01..2010-12")))
NULL

#' @rdname duration_class
#' @export
new_messyduration <- function(x = character()) {
  stopifnot(is.character(x))
  structure(x, class = "mdates_duration")
}

#' @importFrom utils str
#' @export
print.mdates_duration <- function(x, ...) {
  str(x)
}

#' @rdname duration_class
#' @export
messyduration <- function(x, approx_range = 0) UseMethod("messyduration")

#' @rdname duration_class
#' @export
validate_messyduration <- function(x, approx_range = 0) {
  if (any(!grepl("\\.\\.", x))) {
    stop("mdates_duration class objects should have at least one date range",
         call. = FALSE)
  }
}

#' @rdname duration_class
#' @export
messyduration.character <- function(x, approx_range = 0) {
  message("Converting to mdate class.")
  x <- as_messydate(x)
  validate_messyduration(x)
  x <- ifelse(grepl("\\.\\.", x), messy_range(x, approx_range), x)
  new_messyduration(x)
}

#' @rdname duration_class
#' @export
messyduration.mdate <- function(x, approx_range = 0) {
  validate_messyduration(x)
  x <- ifelse(grepl("\\.\\.", x), messy_range(x, approx_range), x)
  new_messyduration(x)
}

messy_range <- function(x, approx_range) {
  dates <- strsplit(x, "\\.\\.")
  dates1 <- as.Date(as_messydate(purrr::map_chr(dates, 1)), FUN = min) + approx_range
  dates2 <- as.Date(as_messydate(purrr::map_chr(dates, 2)), FUN = max) + approx_range
  as_messydate(paste0(dates1, "..", dates2))
}
