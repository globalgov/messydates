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
#' dplyr::mutate(data, Beg = ifelse(Beg <= "1816-01-01",
#'   on_or_before(Beg), Beg))
#' dplyr::mutate(data, End = ifelse(End >= "2016-01-01",
#'   on_or_after(End), End))
#' dplyr::mutate(data, Beg = ifelse(Beg == "1916-01-01",
#'   as_approximate(Beg), Beg))
#' dplyr::mutate(data, End = ifelse(End == "1916-12-31",
#'   as_uncertain(End), End))
#' @name annotate
NULL

#' @describeIn annotate prefixes dates with ".." where start date is uncertain
#' @export
on_or_before <- function(x) {
  x <- paste0("..", x)
  x <- as_messydate(x)
  x
}

#' @describeIn annotate suffixes dates with ".." where end date is uncertain
#' @export
on_or_after <- function(x) {
  x <- paste0(x, "..")
  x <- as_messydate(x)
  x
}

#' @describeIn annotate adds tildes to indicate approximate dates/date components
#' @export
as_approximate <- function(x, component = NULL) {
  if (is.null(component)) {
    x <- paste0(x, "~")
  }
  if (!is.null(component)) {
    day <- vapply(strsplit(x, "-"), `[`, 3, FUN.VALUE = character(1))
    month <- vapply(strsplit(x, "-"), `[`, 2, FUN.VALUE = character(1))
    year <- vapply(strsplit(x, "-"), `[`, 1, FUN.VALUE = character(1))
    if (component == "day") {
      x <- paste0(year, "-", month, "-", "~", day)
    }
    if (component == "month") {
      x <- paste0(year, "-", "~", month, "-", day)
    }
    if (component == "year") {
      x <- paste0("~", year, "-", month, "-", day)
    }
    if (component == "md") {
      x <- paste0(year, "-", "~", month, "-", "~", day)
    }
    if (component == "ym") {
      x <- paste0(year, "-", month, "~", "-", day)
    }
  }
  x <- as_messydate(x)
  x
}

#' @describeIn annotate adds question marks to indicate dubious dates/date components.
#' @export
as_uncertain <- function(x, component = NULL) {
  if (is.null(component)) {
    x <- paste0(x, "?")
  } else {
    day <- vapply(strsplit(x, "-"), `[`, 3,
                  FUN.VALUE = character(1))
    month <- vapply(strsplit(x, "-"), `[`, 2,
                    FUN.VALUE = character(1))
    year <- vapply(strsplit(x, "-"), `[`, 1,
                   FUN.VALUE = character(1))
    if (component == "day") {
      x <- paste0(year, "-", month, "-", "?", day)
    }
    if (component == "month") {
      x <- paste0(year, "-", "?", month, "-", day)
    }
    if (component == "year") {
      x <- paste0("?", year, "-", month, "-", day)
    }
    if (component == "md") {
      x <- paste0(year, "-", "?", month, "-", "?", day)
    }
    if (component == "ym") {
      x <- paste0(year, "-", month, "?", "-", day)
    }
  }
  x <- as_messydate(x)
  x
}
