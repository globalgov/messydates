# Inequalities ####

#' Logical operations on messy dates
#' @param e1,e2 `mdate` or other class objects
#' @name operate_inequalities
NULL

#' @describeIn operate_inequalities tests whether the dates in the first vector precede
#'   the dates in the second vector.
#'   Returns `NA` when the date order can't be determined.
#' @examples
#' as_messydate("2012-06-02") > as.Date("2012-06-01") # TRUE
#' # 2012-06-XX could mean 2012-06-03, so unknown if it comes before 2012-06-02
#' as_messydate("2012-06-XX") < as.Date("2012-06-02") # NA
#' # But 2012-06-XX cannot be before 2012-06-01
#' as_messydate("2012-06-XX") >= as.Date("2012-06-01") # TRUE
#' @export
`<.mdate` <- function(e1, e2) {
  if (!is_messydate(e1)) e1 <- as_messydate(e1)
  if (!is_messydate(e2)) e2 <- as_messydate(e2)
  ranges <- numeric_time_ranges(e1, e2)
  x <- rep(NA, max(length(e1), length(e2)))
  x[ranges[["max1"]] < ranges[["min2"]]] <- TRUE
  x[ranges[["min1"]] > ranges[["max2"]]] <- FALSE
  x[ranges[["max1"]] == ranges[["min2"]]] <- FALSE
  x[ranges[["min1"]] == ranges[["max2"]]] <- FALSE
  x
}

# Quoth the {lubridate} team:
# Nothing else seems to work, only this sneaky trick.
evalqOnLoad({
  registerS3method("<", "Date", `<.mdate`)
  registerS3method("<", "POSIXt", `<.mdate`)
})

numeric_time_ranges <- function(e1, e2) {
  if (is_messydate(e1)) {
    min1 <- as.Date(e1, FUN = vmin)
    max1 <- as.Date(e1, FUN = vmax)
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
    min2 <- as.Date(e2, FUN = vmin)
    max2 <- as.Date(e2, FUN = vmax)
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

#' @describeIn operate_inequalities tests whether the dates in the first vector
#'   succeed the dates in the second vector.
#'   Returns `NA` when the date order can't be determined.
#' @export
`>.mdate` <- function(e1, e2) {
  if (!is_messydate(e1)) e1 <- as_messydate(e1)
  if (!is_messydate(e2)) e2 <- as_messydate(e2)
  ranges <- numeric_time_ranges(e1, e2)
  x <- rep(NA, max(length(e1), length(e2)))
  x[ranges[["min1"]] > ranges[["max2"]]] <- TRUE
  x[ranges[["max1"]] < ranges[["min2"]]] <- FALSE
  x[ranges[["min1"]] == ranges[["max2"]]] <- FALSE
  x[ranges[["max1"]] == ranges[["min2"]]] <- FALSE
  x
}

evalqOnLoad({
  registerS3method(">", "Date", `>.mdate`)
  registerS3method(">", "POSIXt", `>.mdate`)
})

#' @describeIn operate_inequalities tests whether the dates in the first vector are
#'   equal to or precede the dates in the second vector.
#'   Returns `NA` when the date order can't be determined.
#' @export
`<=.mdate` <- function(e1, e2) {
  if (!is_messydate(e1)) e1 <- as_messydate(e1)
  if (!is_messydate(e2)) e2 <- as_messydate(e2)
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

#' @describeIn operate_inequalities tests whether the dates in the first vector are equal to
#'   or succeed the dates in the second vector.
#'   Returns `NA` when the date order can't be determined.
#' @export
`>=.mdate` <- function(e1, e2) {
  if (!is_messydate(e1)) e1 <- as_messydate(e1)
  if (!is_messydate(e2)) e2 <- as_messydate(e2)
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
