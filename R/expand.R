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
#' @examples
#' d <- as_messydate(c("2008-03-25", "2012-02-27", "2001-01?", "2001~",
#' "2001-01-01..2001-02-02", "{2001-01-01,2001-02-02}",
#' "{2001-01,2001-02-02}", "2008-XX-31", "..2002-02-03", "2001-01-03.."))
#' expand(d)
#' @export
expand.messydt <- function(x) {
  x <- stringr::str_remove_all(x, "[:space:]")
  x <- stringr::str_remove_all(x, "\\{")
  x <- stringr::str_remove_all(x, "\\}")
  x <- stringr::str_remove_all(x, "\\%")
  x <- expand_approximate(x)
  x <- expand_unspecified(x)
  x <- expand_sets(x)
  x <- expand_ranges(x)
  x
}

expand_approximate <- function(dates, approx_range = 3) {

  dates <- stringr::str_replace_all(dates, "\\?", "\\~")
  dates <- ifelse(stringr::str_detect(dates, "^[:digit:]{4}\\~$"),
                  paste0(dates, "-01-01"), dates)
  dates <- ifelse(stringr::str_detect(dates, "^[:digit:]{4}-[:digit:]{2}\\~$"),
                  paste0(dates, "-01"), dates)
  dates <- ifelse(stringr::str_detect(dates, "^[:digit:]{4}\\~-[:digit:]{2}-[:digit:]{2}$"),
                  paste0(as.Date(gsub("\\~", "", dates)) - (365*approx_range), "..",
                         as.Date(gsub("\\~", "", dates)) + (365*approx_range)), dates)
  dates <- ifelse(stringr::str_detect(dates, "^[:digit:]{4}\\-[:digit:]{2}\\~-[:digit:]{2}$"),
                  paste0(as.Date(gsub("\\~", "", dates)) - (31*approx_range), "..",
                         as.Date(gsub("\\~", "", dates)) + (31*approx_range)), dates)
  dates <- ifelse(stringr::str_detect(dates, "^[:digit:]{4}-[:digit:]{2}-[:digit:]{2}\\~$"),
                  paste0(as.Date(gsub("\\~", "", dates)) - approx_range, "..",
                         as.Date(gsub("\\~", "", dates)) + approx_range), dates)
  dates <- ifelse(stringr::str_detect(dates, "^\\.\\."),
                  paste0(as.Date(gsub("\\.\\.", "", dates)) - approx_range,
                         "..", gsub("\\.\\.", "", dates)), dates)
  dates <- ifelse(stringr::str_detect(dates, "\\.\\.$"),
                  paste0(gsub("\\.\\.", "", dates), "..",
                         as.Date(gsub("\\.\\.", "", dates)) + approx_range),
                  dates)
  dates <- ifelse(stringr::str_detect(dates, "^[:digit:]{4}-XX-[:digit:]{2}$"),
                  paste0(gsub("XX", "01", dates), "..",
                         gsub("XX", "12", dates)), dates)

  dates
}

expand_unspecified <- function(dates) {
  dates <- stringr::str_replace_all(dates,
                                    "(^|,)([:digit:]{2})($|,)", "\\1\\200-01-01..\\299-12-31\\3")
  dates <- stringr::str_replace_all(dates,
                                    "(^|,)([:digit:]{3})($|,)", "\\1\\20-01-01..\\29-12-31\\3")
  dates <- stringr::str_replace_all(dates,
                                    "(^|,)([:digit:]{4})($|,)", "\\1\\2-01-01..\\2-12-31\\3")
  dates <- stringr::str_replace_all(dates,
                                    "(^|,)([:digit:]{4})-02($|,)", "\\1\\2-02-01..\\2-02-28\\3")
  # needs to correct for leap years
  dates <- stringr::str_replace_all(dates,
                                    "(^|,)([:digit:]{4})-09($|,)", "\\1\\2-09-01..\\2-09-30\\3")
  dates <- stringr::str_replace_all(dates,
                                    "(^|,)([:digit:]{4})-04($|,)", "\\1\\2-04-01..\\2-04-30\\3")
  dates <- stringr::str_replace_all(dates,
                                    "(^|,)([:digit:]{4})-06($|,)", "\\1\\2-06-01..\\2-06-30\\3")
  dates <- stringr::str_replace_all(dates,
                                    "(^|,)([:digit:]{4})-11($|,)", "\\1\\2-11-01..\\2-11-30\\3")
  dates <- stringr::str_replace_all(dates,
                                    "(^|,)([:digit:]{4})-([:digit:]{2})($|,)", "\\1\\2-\\3-01..\\2-\\3-31\\4")
  dates
}

expand_sets <- function(dates) {
  dates <- ifelse(stringr::str_detect(dates, "\\.\\.[:digit:]{4}-[:digit:]{2}-[:digit:]{2}\\,"),
                  stringr::str_replace(dates, "\\.\\.[:digit:]{4}-[:digit:]{2}-[:digit:]{2}", ""),
                  dates)
  dates <- stringr::str_replace_all(dates, "\\,", "\\.\\.")
  dates
}

expand_ranges <- function(dates) {
  dates <- lapply(dates, function(x) {
    x <- stringr::str_split(x, "\\.\\.")
    x <- lapply(x, function(y) {
      if (length(y) == 2) y <- as.character(seq(as.Date(y[1]), as.Date(y[2]), by = "days"))
      y
    })
    unlist(x)
  })
  dates
}
