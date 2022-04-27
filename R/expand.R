#' Expand messy dates to lists of dates
#'
#' These functions expand on date ranges, sets of dates, and unspecified or
#' approximate dates (annotated with '..', '{ , }', 'XX' or '~').
#' As these messydates may refer to several possible dates,
#' the function "opens" these values to reveal a vector of all the possible
#' dates implied.
#' Imprecise dates (dates only containing information on year and/or month)
#' are also expanded to include possible dates within that year and/or month.
#' The function removes the annotation from dates with unreliable sources ('?'),
#' before being expanded normally as though if they were incomplete.
#' @param x A `messydt` object.
#' @param approx_range Range to expand approximate dates,
#' or date components, annotated with '~', by default 3.
#' That is, 3 days for day approximation, 3 months for month approximation,
#' 3 years for year/whole date approximation, 3 years and 3 months for year-
#' month approximation, and 3 months and 3 days for month-day approximation.
#' If 0, returns original values and removes signs for approximate dates.
#' @return A list of dates, including all dates in each range or set.
#' @export
expand <- function(x, approx_range) UseMethod("expand")

#' @describeIn expand Expanding messydates
#' @importFrom stringr str_replace_all str_split str_detect
#' str_extract str_remove_all
#' @importFrom lubridate as_date ymd years
#' @examples
#' d <- as_messydate(c("2008-03-25", "-2012-02-27", "2001-01?", "~2001",
#' "2001-01-01..2001-02-02", "{2001-01-01,2001-02-02}", "{2001-01,2001-02-02}",
#' "2008-XX-31", "..2002-02-03", "2001-01-03..", "28 BC"))
#' expand(d)
#' @export
expand.messydt <- function(x, approx_range = 3) {
  x <- stringr::str_remove_all(x, "[:space:]|\\{|\\}|\\%|\\?")
  x <- suppressWarnings(expand_approximate(x, approx_range))
  x <- expand_unspecified(x)
  x <- expand_negative(x)
  x <- expand_sets(x)
  x <- suppressWarnings(expand_ranges(x))
  x
}

expand_approximate <- function(dates, approx_range) {
  # Substitute signs
  dates <- ifelse(stringr::str_detect(dates, "^\\~[:digit:]{4}$"),
                  paste0(dates, "-01-01"), dates)
  dates <- ifelse(stringr::str_detect(dates, "^[:digit:]{4}-\\~[:digit:]{2}$|^[:digit:]{4}-[:digit:]{2}\\~$"),
                  paste0(dates, "-01"), dates)
  dates <- expand_approximate_years(dates, approx_range)
  dates <- expand_approximate_months(dates, approx_range)
  dates <- expand_approximate_days(dates, approx_range)
  dates <- unlist(dates)
  dates
}

expand_unspecified <- function(dates) {
  dates <- stringr::str_replace_all(dates, ",", ",,")
  dates <- stringr::str_replace_all(dates, "(^|,)([:digit:]{4})($|,)",
                                    "\\1\\2-01-01..\\2-12-31\\3")
  dates <- ifelse(stringr::str_detect(dates, "(^|,)([:digit:]{4})-02($|,)") &
                    !grepl("\\.", as.numeric(stringr::str_extract(dates, "[:digit:]{4}")) / 4),
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
  dates <- stringr::str_replace_all(dates,
                                    "(^|,)([:digit:]{4})-([:digit:]{2})($|,)",
                                    "\\1\\2-\\3-01..\\2-\\3-31\\4")
  dates <- ifelse(stringr::str_detect(dates, "^([:digit:]{2})-([:digit:]{2})-([:digit:]{2}$)") &
                    as.numeric(gsub("-", "", stringr::str_extract(dates, "^[:digit:]{2}"))) < 23,
                  paste0("20", dates), dates)
  dates <- ifelse(stringr::str_detect(dates, "^([:digit:]{2})-([:digit:]{2})-([:digit:]{2}$)") &
                    as.numeric(gsub("-", "", stringr::str_extract(dates, "^[:digit:]{2}"))) > 23,
                  paste0("19", dates), dates)
  dates <- stringr::str_replace_all(dates, ",,", ",")
  dates
}

expand_negative <- function(dates) {
  dates <- stringr::str_replace_all(dates, ",", ",,")
  dates <- stringr::str_replace_all(dates, "(^|,)-([:digit:]{4})($|,)",
                                    "-\\1\\2-01-01%-\\2-12-31\\3")
  dates <- stringr::str_replace_all(dates, "(^|,)-([:digit:]{4})-02($|,)",
                                    "\\1\\2-02-01%\\2-02-28\\3")
  dates <- stringr::str_replace_all(dates, "(^|,)-([:digit:]{4})-04($|,)",
                                    "\\1\\2-04-01%\\2-04-30\\3")
  dates <- stringr::str_replace_all(dates, "(^|,)-([:digit:]{4})-06($|,)",
                                    "\\1\\2-06-01%\\2-06-30\\3")
  dates <- stringr::str_replace_all(dates, "(^|,)-([:digit:]{4})-09($|,)",
                                    "\\1\\2-09-01%\\2-09-30\\3")
  dates <- stringr::str_replace_all(dates, "(^|,)-([:digit:]{4})-11($|,)",
                                    "\\1\\2-11-01%\\2-11-30\\3")
  dates <- stringr::str_replace_all(dates,
                                    "(^|,)-([:digit:]{4})-([:digit:]{2})($|,)",
                                    "\\1\\2-\\3-01%\\2-\\3-31\\4")
  dates <- stringr::str_replace_all(dates, "-,", "-")
  dates <- stringr::str_replace_all(dates, ",,", ",")
  dates
}

expand_sets <- function(dates) {
  # Sets of months
  dates <- ifelse(stringr::str_detect(dates, "^[:digit:]{4}-XX-31$|^[:digit:]{4}-XX-30$"),
                  paste(gsub("XX-31|XX-30", "01-31", dates),
                        gsub("XX-31|XX-30", "02-28", dates),
                        gsub("XX-31|XX-30", "03-31", dates),
                        gsub("XX-31|XX-30", "04-30", dates),
                        gsub("XX-31|XX-30", "05-31", dates),
                        gsub("XX-31|XX-30", "06-30", dates),
                        gsub("XX-31|XX-30", "07-31", dates),
                        gsub("XX-31|XX-30", "08-31", dates),
                        gsub("XX-31|XX-30", "09-30", dates),
                        gsub("XX-31|XX-30", "10-31", dates),
                        gsub("XX-31|XX-30", "11-30", dates),
                        gsub("XX-31|XX-30", "12-31", dates),
                        sep = ","), dates)
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
  dates <- ifelse(stringr::str_detect(dates, "([:digit:]{1})\\.\\.([:digit:]{1})|([:digit:]{1})\\.\\.-") &
                    nchar(dates) < 17,
                  expand_unspecified_ranges(dates), dates)
  dates <- lapply(dates, function(x) {
    x <- stringr::str_split(x, "\\.\\.")
    x <- lapply(x, function(y) {
      if (length(y) == 2) y <- as.character(seq(as.Date(y[1]), as.Date(y[2]),
                                                by = "days"))
      y
    })
    x <- ifelse(stringr::str_detect(x, "\\%"), expand_negative_dates(x), x)
    unlist(x)
  })
  dates
}

expand_negative_dates <- function(dates) {
  dates <- lapply(dates, function(x) {
    x <- stringr::str_split(x, "\\%")
    x <- lapply(x, function(a) stringr::str_remove(a, "^-"))
    x <- lapply(x, function(r) {
      y <- stringr::str_extract(r, "^[0-9]{4}")
      md <- stringr::str_replace(r, "^[0-9]{4}", "0000")
      r <- lubridate::ymd(md) - lubridate::years(y)
      r <- lubridate::as_date(r)
      r
    })
    x <- lapply(x, function(y) {
      if (length(y) == 2) y <- as.character(seq(as.Date(y[1]), as.Date(y[2]),
                                                by = "days"))
      y
    })
    unlist(x)
    x <- lapply(x, function(y) {
      y <- ifelse(nchar(y) == 10, stringr::str_replace_all(y, "^-", "-0"), y)
    })
  })
  dates
}

expand_approximate_years <- function(dates, approx_range) {
  # For year approximation
  ly <- as.numeric(strsplit(as.character(approx_range / 4), "\\.")[[1]][1]) +
    (365 * approx_range)
  dates <- lapply(dates, function(x) {
    # Leap year
    x <- ifelse(stringr::str_detect(x, "^\\~[:digit:]{4}-[:digit:]{2}-[:digit:]{2}$|
                                    |^[:digit:]{4}-[:digit:]{2}-[:digit:]{2}\\~$") &
                  approx_range < 4 &
                  lubridate::leap_year(lubridate::as_date(gsub("\\~", "", x))),
                paste0(as.Date(gsub("\\~", "", x)) - ly, "..",
                       as.Date(gsub("\\~", "", x)) + ly + 1), x)
    # Non leap year
    x <- ifelse(stringr::str_detect(x, "^\\~[:digit:]{4}-[:digit:]{2}-[:digit:]{2}$|
                                    |^[:digit:]{4}-[:digit:]{2}-[:digit:]{2}\\~$"),
                paste0(as.Date(gsub("\\~", "", x)) - ly, "..",
                       as.Date(gsub("\\~", "", x)) + ly), x)
    # On before
    x <- ifelse(stringr::str_detect(x, "^\\.\\."),
                paste0(as.Date(gsub("\\.\\.", "", x)) - ly, "..",
                       gsub("\\.\\.", "", x)), x)
    # On after, leap
    x <- ifelse(stringr::str_detect(x, "\\.\\.$") & approx_range < 4 &
                  lubridate::leap_year(lubridate::as_date(gsub("\\.\\.",
                                                               "", x))),
                paste0(gsub("\\.\\.", "", x), "..",
                       as.Date(gsub("\\.\\.", "", x)) + ly + 1), x)
    # On after
    x <- ifelse(stringr::str_detect(x, "\\.\\.$"),
                paste0(gsub("\\.\\.", "", x), "..", as.Date(gsub("\\.\\.", "", x)) + ly), x)
    # Year-Month
    x <- ifelse(stringr::str_detect(x, "^[:digit:]{4}-[:digit:]{2}\\~-[:digit:]{2}$"),
                paste0(as.Date(gsub("\\~", "", x)) -
                         (ly + (30.42 * approx_range)), "..",
                       as.Date(gsub("\\~", "", x)) +
                         (ly + (30.42 * approx_range))), x)
  })
  dates
}

expand_approximate_months <- function(dates, approx_range) {
  # For month approximation
  mr <- 30.42 * approx_range
  dates <- lapply(dates, function(x) {
    # One Month
    x <- ifelse(approx_range == 1 &
                  stringr::str_detect(x, "-\\~04-|-\\~06-|-\\~09-|-\\~11-"),
                paste0(as.Date(gsub("\\~", "", x)) - 31, "..",
                       as.Date(gsub("\\~", "", x)) + 30), x)
    x <- ifelse(approx_range == 1 &
                  stringr::str_detect(x, "-\\~05-|-\\~10-|-\\~12-|-\\~07-"),
                paste0(as.Date(gsub("\\~", "", x)) - 30, "..",
                       as.Date(gsub("\\~", "", x)) + 31), x)
    x <- ifelse(approx_range == 1 &
                  stringr::str_detect(x, "-\\~01-|-\\~08-"),
                paste0(as.Date(gsub("\\~", "", x)) - 31, "..",
                       as.Date(gsub("\\~", "", x)) + 31), x)
    x <- ifelse(approx_range == 1 &
                  stringr::str_detect(x, "-\\~03-") & # leap year
                  lubridate::leap_year(lubridate::as_date(gsub("\\~", "", x))),
                paste0(as.Date(gsub("\\~", "", x)) - 29, "..",
                       as.Date(gsub("\\~", "", x)) + 31), x)
    x <- ifelse(approx_range == 1 &
                  stringr::str_detect(x, "-\\~03-"),
                paste0(as.Date(gsub("\\~", "", x)) - 28, "..",
                       as.Date(gsub("\\~", "", x)) + 31), x)
    x <- ifelse(approx_range == 1 &
                  stringr::str_detect(x, "-\\~02-") & # leap year
                  lubridate::leap_year(lubridate::as_date(gsub("\\~", "", x))),
                paste0(as.Date(gsub("\\~", "", x)) - 31, "..",
                       as.Date(gsub("\\~", "", x)) + 29), x)
    x <- ifelse(approx_range == 1 & stringr::str_detect(x, "-\\~02-"),
                paste0(as.Date(gsub("\\~", "", x)) - 31, "..",
                       as.Date(gsub("\\~", "", x)) + 28), x)
    # Multiple months
    x <- ifelse(stringr::str_detect(x, "^[:digit:]{4}-\\~[:digit:]{2}-[:digit:]{2}$"),
                paste0(as.Date(gsub("\\~", "", x)) - mr, "..",
                       as.Date(gsub("\\~", "", x)) + mr), x)
    # Month-Day
    x <- ifelse(stringr::str_detect(x, "^[:digit:]{4}-\\~[:digit:]{2}-\\~[:digit:]{2}$"),
                paste0(as.Date(gsub("\\~", "", x)) - (mr + approx_range), "..",
                       as.Date(gsub("\\~", "", x)) + (mr + approx_range)), x)
  })
  dates
}

expand_approximate_days <- function(dates, approx_range) {
  dates <- lapply(dates, function(x) {
    # Day
    x <- ifelse(stringr::str_detect(x, "^[:digit:]{4}-[:digit:]{2}-\\~[:digit:]{2}$"),
                paste0(as.Date(gsub("\\~", "", x)) - approx_range, "..",
                       as.Date(gsub("\\~", "", x)) + approx_range), x)
  })
  dates
}

expand_unspecified_ranges <- function(dates) {
  dates <- strsplit(as.character(dates), "\\.\\.")
  dates1 <- purrr::map_chr(dates, 1)
  dates1 <- ifelse(stringr::str_detect(dates1,
                                       "^([:digit:]{4})$|^-([:digit:]{4})$"),
                  paste0(dates1, "-01-01"), dates1)
  dates1 <- ifelse(stringr::str_detect(dates1, "^([:digit:]{4})-([:digit:]{2})$|^-([:digit:]{4})-([:digit:]{2})$"),
                   paste0(dates1, "-01"), dates1)
  dates2 <- purrr::map_chr(dates, 2)
  dates2 <- ifelse(stringr::str_detect(dates2,
                                       "^([:digit:]{4})$|^-([:digit:]{4})$"),
                   paste0(dates2, "-12-31"), dates2)
  dates2 <- ifelse(stringr::str_detect(dates2, "^([:digit:]{4})-02$|^-([:digit:]{4})-02$"),
                   paste0(dates2, "-28"), dates2)
  dates2 <- ifelse(stringr::str_detect(dates2,
                                       "^([:digit:]{4})-01$|^-([:digit:]{4})-01$|
                                       |^([:digit:]{4})-03$|^-([:digit:]{4})-03$|
                                       |^([:digit:]{4})-05$|^-([:digit:]{4})-05$|
                                       |^([:digit:]{4})-07$|^-([:digit:]{4})-07$|
                                       |^([:digit:]{4})-08$|^-([:digit:]{4})-08$|
                                       |^([:digit:]{4})-10$|^-([:digit:]{4})-10$|
                                       |^([:digit:]{4})-12$|^-([:digit:]{4})-12$"),
                   paste0(dates2, "-31"), dates2)
  dates2 <- ifelse(stringr::str_detect(dates2,
                                       "^([:digit:]{4})-04$|^-([:digit:]{4})-04$|
                                       |^([:digit:]{4})-06$|^-([:digit:]{4})-06$|
                                       |^([:digit:]{4})-09$|^-([:digit:]{4})-09$|
                                       |^([:digit:]{4})-11$|^-([:digit:]{4})-11$"),
                   paste0(dates2, "-30"), dates2)
  dates <- paste(dates1, dates2, sep = "..")
  dates <- ifelse(stringr::str_detect(dates, "^-|\\.\\.-"),
                  gsub("\\.\\.", "%", dates), dates)
  dates
}
