#' Logical tests on messy dates
#'
#' These functions provide various logical tests for messy date objects.
#' @name logical_tests
#' @param x,y,e1,e2 `mdate` or other class objects
#' @return A logical vector the same length as the `mdate` passed.
NULL

#' @describeIn logical_tests tests whether the object inherits the `mdate` class.
#'   If more rigorous validation is required, see `validate_messydate()`.
#' @examples
#' is_messydate(as_messydate("2012-01-01"))
#' is_messydate(as.Date("2012-01-01"))
#' @export
is_messydate <- function(x) {
  inherits(x, "mdate")
}

#' @describeIn logical_tests tests whether there is any intersection between
#'   two messy dates, leveraging `intersect()`.
#' @examples
#' is_intersecting(as_messydate("2012-01"),
#' as_messydate("2012-01-01..2012-02-22"))
#' is_intersecting(as_messydate("2012-01"),
#' as_messydate("2012-02-01..2012-02-22"))
#' @export
is_intersecting <- function(x, y) {
  length(intersect(unlist(expand(x)), unlist(expand(y)))) > 0
}

#' @describeIn logical_tests tests whether one or more messy date can be found
#'   within a messy date range or set.
#' @examples
#' is_subset(as_messydate("2012-01-01"), as_messydate("2012-01"))
#' is_subset(as_messydate("2012-01-01..2012-01-03"), as_messydate("2012-01"))
#' is_subset(as_messydate("2012-01-01"), as_messydate("2012-02"))
#' @export
is_subset <- function(x, y) {
  x <- as.character(expand(x)[[1]])
  y <- as.character(expand(y)[[1]])
  any(is.element(x, y))
}

#' @describeIn logical_tests tests whether two dates contain similar components.
#'   This can be useful for identifying dates that may be typos of one another.
#' @examples
#' is_similar(as_messydate("2012-06-02"), as_messydate("2012-02-06"))
#' is_similar(as_messydate("2012-06-22"), as_messydate("2012-02-06"))
#' @export
is_similar <- function(x, y) {
  year(x) == year(y) & month(x) == day(y) & day(x) == month(y)
}

#' @describeIn logical_tests tests whether a date is precise (i.e. an 8 digit date).
#'   Non-precise dates contain markers that they are approximate (i.e. ~),
#'   unreliable (i.e. ?), are incomplete dates (i.e. year only),
#'   or date ranges and sets.
#' @examples
#' is_precise(as_messydate(c("2012-06-02", "2012-06")))
#' @export
is_precise <- function(x) {
  stringr::str_detect(x, "^[:digit:]{4}-[:digit:]{2}-[:digit:]{2}$|
                      |^-[:digit:]{4}-[:digit:]{2}-[:digit:]{2}$")
}

#' @describeIn logical_tests tests whether a date is uncertain (i.e. contains ?).
#' @examples
#' is_uncertain(as_messydate(c("2012-06-02", "2012-06-02?")))
#' @export
is_uncertain <- function(x) {
  stringr::str_detect(x, "\\?")
}

#' @describeIn logical_tests tests whether a date is approximate (i.e. contains ~).
#' @examples
#' is_approximate(as_messydate(c("2012-06-02~", "2012-06-02")))
#' @export
is_approximate <- function(x) {
  stringr::str_detect(x, "\\~")
}

#' @describeIn logical_tests tests whether the dates in the first vector precede
#'   the dates in the second vector.
#'   Returns `NA` when the date order can't be determined.
#' @examples
#' # 2012-06-XX could mean 2012-06-03, so unknown if it comes before 2012-06-02
#' as_messydate("2012-06-XX") < as.Date("2012-06-02") # NA
#' # But 2012-06-XX cannot be before 2012-06-01
#' as_messydate("2012-06-XX") >= as.Date("2012-06-01") # TRUE
#' @export
`<.mdate` <- function(e1, e2) {
  if (!is_messydate(e1) && !is_messydate(e2)) {
    nums <- comparable_numerics(e1, e2)
    return(as.numeric(nums[[1]]) < as.numeric(nums[[2]]))
  }
  ranges <- numeric_time_ranges(e1, e2)
  x <- rep(NA, max(length(e1), length(e2)))
  x[ranges[["max1"]] < ranges[["min2"]]] <- TRUE
  x[ranges[["min1"]] > ranges[["max2"]]] <- FALSE
  x
}

# Quoth the {lubridate} team:
# Nothing else seems to work, only this sneaky trick.
evalqOnLoad({
  registerS3method("<", "Date", `<.mdate`)
  registerS3method("<", "POSIXt", `<.mdate`)
})

# Converting to numbers before comparison prevents infinite loops.
# But make sure both are counting seconds, or both are counting days.
comparable_numerics <- function(e1, e2) {
  if (!lubridate::is.POSIXt(e1) && lubridate::is.POSIXt(e2)) {
    e1 <- lubridate::force_tz(e1, lubridate::tz(e2))
    e1 <- as.POSIXct(e1)
  } else if (lubridate::is.POSIXt(e1) && !lubridate::is.POSIXt(e2)) {
    e2 <- lubridate::force_tz(e2, lubridate::tz(e1))
    e2 <- as.POSIXct(e2)
  }
  list(as.numeric(e1), as.numeric(e2))
}

numeric_time_ranges <- function(e1, e2) {
  if (is_messydate(e1)) {
    min1 <- as.Date(e1, FUN = min)
    max1 <- as.Date(e1, FUN = max)
    if (lubridate::is.POSIXt(e2)) {
      ptz <- lubridate::tz(e2)
      min1 <- lubridate::force_tz(min1, ptz)
      min1 <- as.POSIXct(min1)
      max1 <- lubridate::force_tz(max1, ptz)
      max1 <- as.POSIXct(max1)
    }
  } else {
    min1 <- max1 <- e1
  }
  if (is_messydate(e2)) {
    min2 <- as.Date(e2, FUN = min)
    max2 <- as.Date(e2, FUN = max)
    if (lubridate::is.POSIXt(e1)) {
      ptz <- lubridate::tz(e1)
      min2 <- lubridate::force_tz(min2, ptz)
      min2 <- as.POSIXct(min2)
      max2 <- lubridate::force_tz(max2, ptz)
      max2 <- as.POSIXct(max2)
    }
  } else {
    min2 <- max2 <- e2
  }
  list(
    min1 = as.numeric(min1), max1 = as.numeric(max1),
    min2 = as.numeric(min2), max2 = as.numeric(max2)
  )
}

#' @describeIn logical_tests tests whether the dates in the first vector
#'   succeed the dates in the second vector.
#'   Returns `NA` when the date order can't be determined.
#' @export
`>.mdate` <- function(e1, e2) {
  if (!is_messydate(e1) && !is_messydate(e2)) {
    nums <- comparable_numerics(e1, e2)
    return(as.numeric(nums[[1]]) > as.numeric(nums[[2]]))
  }
  ranges <- numeric_time_ranges(e1, e2)
  x <- rep(NA, max(length(e1), length(e2)))
  x[ranges[["min1"]] > ranges[["max2"]]] <- TRUE
  x[ranges[["max1"]] < ranges[["min2"]]] <- FALSE
  x
}

evalqOnLoad({
  registerS3method(">", "Date", `>.mdate`)
  registerS3method(">", "POSIXt", `>.mdate`)
})

#' @describeIn logical_tests tests whether the dates in the first vector are
#'   equal to or precede the dates in the second vector.
#'   Returns `NA` when the date order can't be determined.
#' @export
`<=.mdate` <- function(e1, e2) {
  if (!is_messydate(e1) && !is_messydate(e2)) {
    nums <- comparable_numerics(e1, e2)
    return(as.numeric(nums[[1]]) <= as.numeric(nums[[2]]))
  }
  ranges <- numeric_time_ranges(e1, e2)
  x <- rep(NA, max(length(e1), length(e2)))
  x[ranges[["max1"]] <= ranges[["min2"]]] <- TRUE
  x[ranges[["min1"]] > ranges[["max2"]]] <- FALSE
  x
}

evalqOnLoad({
  registerS3method("<=", "Date", `<=.mdate`)
  registerS3method("<=", "POSIXt", `<=.mdate`)
})

#' @describeIn logical_tests tests whether the dates in the first vector are equal to
#'   or succeed the dates in the second vector.
#'   Returns `NA` when the date order can't be determined.
#' @export
`>=.mdate` <- function(e1, e2) {
  if (!is_messydate(e1) && !is_messydate(e2)) {
    nums <- comparable_numerics(e1, e2)
    return(as.numeric(nums[[1]]) >= as.numeric(nums[[2]]))
  }
  ranges <- numeric_time_ranges(e1, e2)
  x <- rep(NA, max(length(e1), length(e2)))
  x[ranges[["min1"]] >= ranges[["max2"]]] <- TRUE
  x[ranges[["max1"]] < ranges[["min2"]]] <- FALSE
  x
}

evalqOnLoad({
  registerS3method(">=", "Date", `>=.mdate`)
  registerS3method(">=", "POSIXt", `>=.mdate`)
})
