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
expand <- function(x, approx_range) UseMethod("expand")

#' @describeIn expand Expanding messydates
#' @importFrom stringr str_replace_all str_split str_detect
#' str_extract str_remove_all
#' @param approx_range range to expand approximate dates to, by default 3.
#' That is, 3 days for day approximation, 3 months for month approximation, and
#' 3 years for year approximation.
#' @examples
#' d <- as_messydate(c("2008-03-25", "2012-02-27", "2001-01?", "2001~",
#' "2001-01-01..2001-02-02", "{2001-01-01,2001-02-02}",
#' "{2001-01,2001-02-02}", "2008-XX-31", "..2002-02-03", "2001-01-03..", "28 BC"))
#' expand(d)
#' @export
expand.messydt <- function(x, approx_range = 3) {
  x <- stringr::str_remove_all(x, "[:space:]")
  x <- stringr::str_remove_all(x, "\\{")
  x <- stringr::str_remove_all(x, "\\}")
  x <- stringr::str_remove_all(x, "\\%")
  x <- stringr::str_remove_all(x, "\\?") # Dubious dates are not expanded...
  x <- expand_approximate(x, approx_range)
  x <- expand_unspecified(x)
  x <- expand_negative(x) # Negative date rages are treated as sets for now...
  x <- expand_sets(x)
  x <- expand_ranges(x)
  x
}

expand_approximate <- function(dates, approx_range) {
  # For year approximation
  ar <- as.numeric(strsplit(as.character(approx_range/4), "\\.")[[1]][1])
  ly = ar + (365*approx_range)
  # For month approximation
  mr <- 30.42*approx_range
  # Substitute signs
  dates <- ifelse(stringr::str_detect(dates, "^[:digit:]{4}\\~$"),
                  paste0(dates, "-01-01"), dates)
  dates <- ifelse(stringr::str_detect(dates, "^[:digit:]{4}-[:digit:]{2}\\~$"),
                  paste0(dates, "-01"), dates)
  # Year
  dates <- ifelse(stringr::str_detect(dates, "^[:digit:]{4}\\~-[:digit:]{2}-[:digit:]{2}$|
                                      |^\\~[:digit:]{4}\\-[:digit:]{2}-[:digit:]{2}$"),
                  paste0(as.Date(gsub("\\~", "", dates)) - ly, "..",
                         as.Date(gsub("\\~", "", dates)) + ly), dates)
  # Month
  dates <- ifelse(stringr::str_detect(dates, "^[:digit:]{4}\\-[:digit:]{2}\\~-[:digit:]{2}$"),
                  paste0(as.Date(gsub("\\~", "", dates)) - mr, "..",
                         as.Date(gsub("\\~", "", dates)) + mr), dates)
  # Day
  dates <- ifelse(stringr::str_detect(dates, "^[:digit:]{4}-[:digit:]{2}-[:digit:]{2}\\~$"),
                  paste0(as.Date(gsub("\\~", "", dates)) - approx_range, "..",
                         as.Date(gsub("\\~", "", dates)) + approx_range), dates)
  # On before and after
  dates <- ifelse(stringr::str_detect(dates, "^\\.\\."),
                  paste0(as.Date(gsub("\\.\\.", "", dates)) - ly,
                         "..", gsub("\\.\\.", "", dates)), dates)
  dates <- ifelse(stringr::str_detect(dates, "\\.\\.$"),
                  paste0(gsub("\\.\\.", "", dates), "..",
                         as.Date(gsub("\\.\\.", "", dates)) + ly), dates)
  dates
}

expand_unspecified <- function(dates) {
  dates <- stringr::str_replace_all(dates, "(^|,)([:digit:]{2})($|,)",
                                    "\\1\\200-01-01..\\299-12-31\\3")
  dates <- stringr::str_replace_all(dates, "(^|,)([:digit:]{3})($|,)",
                                    "\\1\\20-01-01..\\29-12-31\\3")
  dates <- stringr::str_replace_all(dates, "(^|,)([:digit:]{4})($|,)",
                                    "\\1\\2-01-01..\\2-12-31\\3")
  dates <- ifelse(stringr::str_detect(dates, "(^|,)([:digit:]{4})-02($|,)") &
                    !grepl("\\.", as.numeric(stringr::str_extract(dates, "[:digit:]{4}"))/4),
                  stringr::str_replace_all(dates, "(^|,)([:digit:]{4})-02($|,)",
                                           "\\1\\2-02-01..\\2-02-29\\3"),
                  stringr::str_replace_all(dates, "(^|,)([:digit:]{4})-02($|,)",
                                           "\\1\\2-02-01..\\2-02-28\\3"))
  dates <- stringr::str_replace_all(dates, "(^|,)([:digit:]{4})-09($|,)",
                                    "\\1\\2-09-01..\\2-09-30\\3")
  dates <- stringr::str_replace_all(dates, "(^|,)([:digit:]{4})-04($|,)",
                                    "\\1\\2-04-01..\\2-04-30\\3")
  dates <- stringr::str_replace_all(dates, "(^|,)([:digit:]{4})-06($|,)",
                                    "\\1\\2-06-01..\\2-06-30\\3")
  dates <- stringr::str_replace_all(dates, "(^|,)([:digit:]{4})-11($|,)",
                                    "\\1\\2-11-01..\\2-11-30\\3")
  dates <- stringr::str_replace_all(dates, "(^|,)([:digit:]{4})-([:digit:]{2})($|,)",
                                    "\\1\\2-\\3-01..\\2-\\3-31\\4")
  dates
}

expand_negative <- function(dates) {
  dates <- stringr::str_replace_all(dates, "(^|,)-([:digit:]{4})($|,)",
                                    "-\\1\\2-01-01,-\\2-12-31\\3")
  dates <- stringr::str_replace_all(dates, "(^|,)-([:digit:]{4})-02($|,)",
                                    "\\1\\2-02-01,\\2-02-28\\3")
  dates <- stringr::str_replace_all(dates, "(^|,)-([:digit:]{4})-04($|,)",
                                    "\\1\\2-04-01,\\2-04-30\\3")
  dates <- stringr::str_replace_all(dates, "(^|,)-([:digit:]{4})-06($|,)",
                                    "\\1\\2-06-01,\\2-06-30\\3")
  dates <- stringr::str_replace_all(dates, "(^|,)-([:digit:]{4})-09($|,)",
                                    "\\1\\2-09-01,\\2-09-30\\3")
  dates <- stringr::str_replace_all(dates, "(^|,)-([:digit:]{4})-11($|,)",
                                    "\\1\\2-11-01,\\2-11-30\\3")
  dates <- stringr::str_replace_all(dates, "(^|,)-([:digit:]{4})-([:digit:]{2})($|,)",
                                    "\\1\\2-\\3-01,\\2-\\3-31\\4")
  dates
}

expand_sets <- function(dates) {
  # Sets of months
  dates <- ifelse(stringr::str_detect(dates, "^[:digit:]{4}-XX-[:digit:]{2}$"),
                  paste(gsub("XX", "01", dates), gsub("XX", "02", dates),
                        gsub("XX", "03", dates), gsub("XX", "04", dates),
                        gsub("XX", "05", dates), gsub("XX", "06", dates),
                        gsub("XX", "07", dates), gsub("XX", "08", dates),
                        gsub("XX", "09", dates), gsub("XX", "10", dates),
                        gsub("XX", "11", dates), gsub("XX", "12", dates),
                        sep = ","), dates)
  dates <- stringr::str_split(dates, "\\,")
  dates
}

expand_ranges <- function(dates) {
  dates <- lapply(dates, function(x) {
    x <- stringr::str_split(x, "\\.\\.")
    x <- lapply(x, function(y) {
      if (length(y) == 2) y <- as.character(seq(as.Date(y[1]), as.Date(y[2]),
                                                by = "days"))
      y
    })
    unlist(x)
  })
  dates
}
