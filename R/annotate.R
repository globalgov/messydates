#' Annotates messy dates for uncertainty and/or approximation
#'
#' In some cases, datasets have an arbitrary cut off point
#' for start and end points, for example,
#' but these do not refer to the real start or end dates.
#' Though these are coded as precise dates.
#' This collection of functions helps annotate uncertainty and
#' approximation to dates according to ISO2019E standards.
#' Uncertain start or end dates can be represented by an affix
#' indicating "on or before", if used as a prefix (e.g. ..1816-01-01),
#' or indicating "on or after", if used as a suffix (e.g. 2016-12-31..).
#' Approximate dates are indicated by adding a `~` to year,
#' month, or day, to estimate whose value is asserted
#' to be possibly correct (e.g. 2003~-03-03).
#' Specific day, month, or year, uncertainty can be indicated by `?`,
#' added to a date component whose source is considered
#' to be dubious (e.g. 1916?-10-10).
#' @param data Dataset
#' @param var Variable
#' @param date Date to be annotated.
#' For `on_or_before()` and `on_or_after()` all dates before/after,
#' or equal, the specified date value are annotated in variable.
#' For other annotate functions, all dates corresponding to
#' specified date value in variable are annotated.
#' @param level Should the annotation be on the "year", "month", or "day"?
#' Optional. If unspecified annotation will be added to before the date.
#' @return A varaible vector with annotated date(s)
#' @examples
#' data <- data.frame(Beg = c("1816-01-01", "1916-01-01", "2016-01-01"),
#' End = c("1816-12-31", "1916-12-31", "2016-12-31"))
#' on_or_before(data, "Beg", "1816-01-01")
#' on_or_after(data, "End", "2016-12-31")
#' @name annotate
NULL

#' @rdname annotate
#' @details `on_or_before()` annotates uncertain start dates by adding ".." as a prefix to dates(e.g. `..2019-01-01`)
#' @export
on_or_before <- function(data, var = NULL, date = NULL) {

  if (missing(data)) {
    stop("Please declare a dataset")
  }

  if (is.null(var) & is.null(date)) {
    var = "Beg"
    date = "1816-01-01"
    print("Varaible set to Beg and date set to 1816-01-01")
  }

d <- ifelse(data[[var]] <= date, paste0("..", data[[var]]), data[[var]])
d
}

#' @rdname annotate
#' @export
on_or_after <- function(data, var = NULL, date = NULL) {

  if (missing(data)) {
    stop("Please declare a dataset")
  }

  if (is.null(var) & is.null(date)) {
    var = "End"
    date = "2016-12-31"
    print("Varaible set to End and date set to 2016-12-31")
  }

  d <- ifelse(data[[var]] >= date, paste0(data[[var]], ".."), data[[var]])
  d
}

#' @rdname annotate
#' @export
add_approximation <- function(data, var = NULL, date = NULL, level = NULL) {

  if (missing(data)) {
    stop("Please declare a dataset")
  }

  if (is.null(var)) {
    stop("Please declare a variable")
  }

  if (is.null(var)) {
    stop("Please declare a date")
  }

  if (is.null(level)) {
    d <- ifelse(data[[var]] == date, paste0("~", data[[var]]), data[[var]])
  }

  if (!is.null(level)) {
    day <- strsplit(date, "-")[[1]][3]
    month <- strsplit(date, "-")[[1]][2]
    year <- strsplit(date, "-")[[1]][1]
    if (level == "day") {
      d <- ifelse(data[[var]] == date, paste0(year, "-", month, "-", day, "~"), data[[var]])
    }
    if (level == "month") {
      d <- ifelse(data[[var]] == date, paste0(year, "-", month, "~", "-", day), data[[var]])
    }
    if (level == "year") {
      d <- ifelse(data[[var]] == date, paste0(year, "~", "-", month, "-", day), data[[var]])
    }
  }
d
}

#' @rdname annotate
#' @export
add_uncertainty <- function(data, var = NULL, date = NULL, level = NULL) {

  if (missing(data)) {
    stop("Please declare a dataset")
  }

  if (is.null(var)) {
    stop("Please declare a variable")
  }

  if (is.null(var)) {
    stop("Please declare a date")
  }

  if (is.null(level)) {
    d <- ifelse(data[[var]] == date, paste0("?", data[[var]]), data[[var]])
  }

  if (!is.null(level)) {
    day <- strsplit(date, "-")[[1]][3]
    month <- strsplit(date, "-")[[1]][2]
    year <- strsplit(date, "-")[[1]][1]
    if (level == "day") {
      d <- ifelse(data[[var]] == date, paste0(year, "-", month, "-", day, "?"), data[[var]])
    }
    if (level == "month") {
      d <- ifelse(data[[var]] == date, paste0(year, "-", month, "?", "-", day), data[[var]])
    }
    if (level == "year") {
      d <- ifelse(data[[var]] == date, paste0(year, "?", "-", month, "-", day), data[[var]])
    }
  }
  d
}
