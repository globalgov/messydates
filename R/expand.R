#' Expand messy dates to lists of dates
#'
#' These functions expand on date ranges, sets of dates, and uncertain or
#' approximate dates (annotated with ? or ~).
#' Uncertain dates may refer to several possible dates.
#' The function "opens" these values to include all the possible dates
#' implied by uncertain dates.
#' Imprecise dates (dates only containing information on year and/or month)
#' are also expanded to include possible dates in that year and/or month.
#' @param x A `messydt` object.
#' @return A list of dates, including all dates in each range or set.
#' @export
expand <- function(x) UseMethod("expand")

#' @describeIn expand Expanding messydates
#' @importFrom stringr str_replace_all str_split str_detect str_extract str_remove_all
#' lubridate leap_year
#' @examples
#' d <- as_messydate(c("2008-03-25", "?2012-02-27", "2001-01?", "2001~",
#' "2001-01-01..2001-02-02", "{2001-01-01,2001-02-02}",
#' "{2001-01,2001-02-02}", "2008-XX-31", "..2002-02-03", "2001-01-03.."))
#' expand(d)
#' @export
expand.messydt <- function(x) {

  x <- remove_qualifiers(x)
  # x <- expand_unspecified(x)
  x <- expand_sets(x)
  x <- expand_imprecise(x)
  x <- expand_approximate(x)
  x <- expand_range(x)

  x
}

remove_qualifiers <- function(dates) {
  # dates <- stringr::str_remove_all(dates, "\\~")
  # dates <- stringr::str_remove_all(dates, "\\%")
  dates <- stringr::str_remove_all(dates, "\\?")
  dates <- stringr::str_remove_all(dates, "[:space:]")
  dates <- stringr::str_remove_all(dates, "\\{")
  dates <- stringr::str_remove_all(dates, "\\}")
  dates
}

expand_sets <- function(dates) {
  dates <- stringr::str_split(x, ",")
  dates
}

expand_imprecise <- function(dates) {
  if(stringr::str_detect(dates, "\\.\\.") == FALSE) {
    dates <- stringr::str_split(dates, "-")
    for (k in seq_len(length(dates))) {
      y <- dates[[k]][1]
      m <- dates[[k]][2]
      d <- dates[[k]][3]
      dates <- ifelse(lengths(dates[[k]])==2, paste0(y, "-", m, "-01..", y, "-", m, "-31"),
                      ifelse(lengths(dates[[k]])==1, paste0(y, "-01-01..-12-31"),
                             paste0(y, "-", m, "-", d)))
      dates
    }
  }
  else {
    paste0(dates)
  }
}

expand_approximate <- function(dates, approx_range = 3) {

  dates <- ifelse(stringr::str_detect(dates, "~"), stringr::str_split(dates, "-"), dates)
  if (lengths(dates) > 1) {
    for (i in seq_len(length(dates))) {
      y <- dates[[i]][1]
      m <- dates[[i]][2]
      d <- dates[[i]][3]

      dates <- ifelse(stringr::str_detect(dates[[i]], "^[:digit:]{4}~-"),
                      paste0(as.numeric(gsub("~", "", y))-(approx_range+1), "-", m, "-", d, "..", as.numeric(gsub("~", "", y))+approx_range+1, "-", m, "-", d),
                      ifelse(stringr::str_detect(dates[[i]], "-[:digit:]{2}~-"),
                             paste0(y, "-", as.numeric(gsub("~", "", m))-(approx_range+1), "-", d, "..", y, as.numeric(gsub("~", "", m))+approx_range+1, "-", d),
                             paste0(y, "-", m, "-", as.numeric(gsub("~", "", d))-(approx_range+1), "..", y, "-", m, "-", as.numeric(gsub("~", "", d))+approx_range+1)))
      dates
    }
  }
  else {
    paste0(dates)
  }
}

expand_range <- function(dates, approx_range = 3) {

  dates <- lapply(dates, function(x) {
    # expand unspecified
    if(stringr::str_detect(x, "^([:digit:]{4})-XX-([:digit:]{2})$") == TRUE) {
      x <- rep(x, each = 12)
      y <- as.character(seq(from = 1, length.out = 12))
      y <- paste0("-", y, "-")
      y <- rep(y, times = 7)
      x <- as.Date(stringr::str_replace_all(x, "-XX-", y))
      x <- na.omit(as.character(x))
      x
    }
    # expand ranges
    else {
      if (stringr::str_detect(x, "\\.\\.")) {
        if (stringr::str_detect(x, "^..[:digit:]") ==  TRUE) {
          x <- stringr::str_remove_all(x, "\\.\\.")
          x <- as.character(seq(from = as.Date(x), by = "-1 day", length.out = approx_range+1))
        }
        else {
          if (stringr::str_detect(x, "[:digit:]..$") == TRUE) {
            x <- stringr::str_remove_all(x, "\\.\\.")
            x <- as.character(seq(from = as.Date(x), by = "days", length.out = approx_range+1))
          }
          else {
            x <- stringr::str_split(x, "\\.\\.")
            x <- as.character(seq(as.Date(x[1]), as.Date(x[2]), by = "days"))
          }
        }
      }
      else {
        paste0(x)
      }
    }
    unlist(x)
  })
  dates
}
