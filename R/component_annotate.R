#' Annotates dates as censored, uncertain, or approximate
#'
#' Some datasets have for example an arbitrary cut off point
#' for start and end points, but these are often coded as precise dates
#' when they are not necessarily the real start or end dates.
#' This collection of functions helps annotate uncertainty and
#' approximation to dates according to ISO2019E standards.
#' Inaccurate start or end dates can be represented by an affix
#' indicating "on or before", if used as a prefix (e.g. `..1816-01-01`),
#' or indicating "on or after", if used as a suffix (e.g. `2016-12-31..`).
#' Approximate dates are indicated by adding a tilde to year,
#' month, or day components, as well as groups of components or whole dates
#' to estimate values that are possibly correct (e.g. `2003-03-03~`).
#' Day, month, or year, uncertainty can be indicated by adding a question mark
#' to a possibly dubious date (e.g. `1916-10-10?`) or date
#' component (e.g. `1916-?10-10`).
#' @param x A date vector
#' @param component Annotation can be added on specific date components
#' ("year", "month" or "day"), or to groups of date components (month and
#' day ("md"), or year and month ("ym")). This must be specified.
#' If unspecified, annotation will be added after the date (e.g. `1916-10-10?`),
#' indicating the whole date is uncertain or approximate.
#' For specific date components, uncertainty or approximation is annotated to
#' the left of the date component.
#' E.g. for "day": `1916-10-?10` or `1916-10-~10`.
#' For groups of date components, uncertainty or approximation is annotated to
#' the right of the group ("ym") or to both components ("md").
#' E.g. for "ym": `1916-10~-10`; for "md": `1916-?10-?10`.
#' @return A `mdate` object with annotated date(s)
#' @examples
#' data <- data.frame(Beg = c("1816-01-01", "1916-01-01", "2016-01-01"),
#'   End = c("1816-12-31", "1916-12-31", "2016-12-31"))
#' transform(data, Beg = ifelse(Beg <= "1816-01-01",
#'   as.character(on_or_before(Beg)), Beg))
#' transform(data, End = ifelse(End >= "2016-01-01",
#'   as.character(on_or_after(End)), End))
#' transform(data, Beg = ifelse(Beg == "1916-01-01",
#'   as.character(as_approximate(Beg)), Beg))
#' transform(data, End = ifelse(End == "1916-12-31",
#'   as.character(as_uncertain(End)), End))
#' @name component_annotate
NULL

#' @describeIn component_annotate prefixes dates with ".." where start date is uncertain
#' @export
on_or_before <- function(x) {
  x <- paste0("..", x)
  x <- as_messydate(x)
  x
}

#' @describeIn component_annotate suffixes dates with ".." where end date is uncertain
#' @export
on_or_after <- function(x) {
  x <- paste0(x, "..")
  x <- as_messydate(x)
  x
}

#' @describeIn component_annotate adds tildes to indicate approximate dates/date components
#' @details
#'   For date-times, `component` may also be "hour", "minute", or "second"
#'   (the marker is placed to the left of that time component), or "time"
#'   (the whole time of day is marked).
#' @export
as_approximate <- function(x, component = NULL) {
  annotate_component(x, component, "~")
}

#' @describeIn component_annotate adds question marks to indicate dubious dates/date components.
#' @export
as_uncertain <- function(x, component = NULL) {
  annotate_component(x, component, "?")
}

# Inserts an annotation marker (~ or ?) on a named component of a date or
# date-time. With no component the whole value is marked.
annotate_component <- function(x, component, mark) {
  x <- as.character(x)
  if (is.null(component)) return(as_messydate(paste0(x, mark)))
  has_t <- grepl("T", x)
  date <- sub("T.*$", "", x)
  time <- ifelse(has_t, sub("^[^T]*T", "", x), "")
  if (component %in% c("year", "month", "day", "md", "ym")) {
    dp <- strsplit(date, "-")
    year <- vapply(dp, `[`, character(1), 1)
    month <- vapply(dp, `[`, character(1), 2)
    day <- vapply(dp, `[`, character(1), 3)
    date <- switch(component,
      day   = paste0(year, "-", month, "-", mark, day),
      month = paste0(year, "-", mark, month, "-", day),
      year  = paste0(mark, year, "-", month, "-", day),
      md    = paste0(year, "-", mark, month, "-", mark, day),
      ym    = paste0(year, "-", month, mark, "-", day))
  } else if (component %in% c("hour", "minute", "second", "time")) {
    tp <- strsplit(time, ":")
    hh <- vapply(tp, `[`, character(1), 1)
    mm <- vapply(tp, `[`, character(1), 2)
    ss <- vapply(tp, `[`, character(1), 3)
    opt <- function(v) ifelse(is.na(v), "", paste0(":", v))
    time <- switch(component,
      hour   = paste0(mark, hh, opt(mm), opt(ss)),
      minute = paste0(hh, ":", mark, mm, opt(ss)),
      second = paste0(hh, ":", mm, ":", mark, ss),
      time   = paste0(time, mark))
  } else {
    stop("Unknown component: ", component, call. = FALSE)
  }
  out <- ifelse(has_t, paste0(date, "T", time), date)
  as_messydate(out)
}
