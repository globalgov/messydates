#' Annotates messy dates for uncertainty and/or approximation
#'
#' Some datasets have an arbitrary cut off point
#' for start and end points, for example,
#' but these do not refer to the real start or end dates.
#' Though these are often coded as precise dates.
#' This collection of functions helps annotate uncertainty and
#' approximation to dates according to ISO2019E standards.
#' Innacurate start or end dates can be represented by an affix
#' indicating "on or before", if used as a prefix (e.g. ..1816-01-01),
#' or indicating "on or after", if used as a suffix (e.g. 2016-12-31..).
#' Approximate dates are indicated by adding a `~` to year,
#' month, or day, to estimate values that are
#' possibly correct (e.g. 2003~-03-03).
#' Day, month, or year, uncertainty can be indicated by adding a `?`
#' to a possibly dubious date (e.g. ?1916-10-10) or date
#' component (e.g. 1916-10-10?).
#' @param x A date vector
#' @param component Annotation can be added on the "year", "month", "day",
#' or month and day ("md"), must be specified.
#' If unspecified annotation will be added to before date.
#' For month and day ("md") uncertainty or approximation
#' sign is added before month in date (e.g. 1916-?10-10 or 1916-~10-10).
#' @return A messydt object with annotated date(s)
#' @examples
#' data <- data.frame(Beg = c("1816-01-01", "1916-01-01", "2016-01-01"),
#' End = c("1816-12-31", "1916-12-31", "2016-12-31"))
#' dplyr::mutate(data, Beg = ifelse(Beg <= "1816-01-01",
#'  on_or_before(Beg), Beg))
#' dplyr::mutate(data, End = ifelse(End >= "2016-01-01",
#'  on_or_after(End), End))
#' dplyr::mutate(data, Beg = ifelse(Beg == "1916-01-01",
#' as_approximate(Beg), Beg))
#' dplyr::mutate(data, End = ifelse(End == "1916-12-31",
#' as_uncertain(End), End))
#' @name annotate
NULL

#' @rdname annotate
#' @details `on_or_before()` annotates uncertain start dates by adding ".." as
#' a prefix to dates (e.g. `..1816-01-01`).
#' @export
on_or_before <- function(x) {
x <- paste0("..", x)
x <- as_messydate(x)
x
}

#' @rdname annotate
#' @details `on_or_after()` annotates uncertain end dates by adding ".." as
#' a suffix to dates (e.g. `2016-12-31`).
#' @export
on_or_after <- function(x) {
  x <- paste0(x, "..")
  x <- as_messydate(x)
  x
}

#' @rdname annotate
#' @details `as_approximate()` annotates approximate dates, or date
#' components, deemed possibly correct by adding "~" to date
#' (e.g. `1916~-01-01`)
#' @export
as_approximate <- function(x, component = NULL) {

  if (is.null(component)) {
    x <- paste0("~", x)
  }

  if (!is.null(component)) {
    day <- vapply(strsplit(x, "-"), `[`, 3, FUN.VALUE = character(1))
    month <- vapply(strsplit(x, "-"), `[`, 2, FUN.VALUE = character(1))
    year <- vapply(strsplit(x, "-"), `[`, 1, FUN.VALUE = character(1))
    if (component == "day") {
      x <- paste0(year, "-", month, "-", day, "~")
    }
    if (component == "month") {
      x <- paste0(year, "-", month, "~", "-", day)
    }
    if (component == "year") {
      x <- paste0(year, "~", "-", month, "-", day)
    }
    if (component == "md") {
      x <- paste0(year, "-", "~", month, "-", day)
    }
  }
  x <- as_messydate(x)
  x
}

#' @rdname annotate
#' @details `as_uncertain()` annotates uncertain dates, or date components,
#' deemed dubious by adding "?" to date (e.g. `1916?-01-01`)
#' @export
as_uncertain <- function(x, component = NULL) {

  if (is.null(component)) {
    x <- paste0("?", x)
  }

  if (!is.null(component)) {
    day <- vapply(strsplit(x, "-"), `[`, 3, FUN.VALUE = character(1))
    month <- vapply(strsplit(x, "-"), `[`, 2, FUN.VALUE = character(1))
    year <- vapply(strsplit(x, "-"), `[`, 1, FUN.VALUE = character(1))
    if (component == "day") {
      x <- paste0(year, "-", month, "-", day, "?")
    }
    if (component == "month") {
      x <- paste0(year, "-", month, "?", "-", day)
    }
    if (component == "year") {
      x <- paste0(year, "?", "-", month, "-", day)
    }
    if (component == "md") {
      x <- paste0(year, "-", "?", month, "-", day)
    }
  }
  x <- as_messydate(x)
  x
}
