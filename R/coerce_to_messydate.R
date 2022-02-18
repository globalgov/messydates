#' Coercion to messy dates
#'
#' These functions coerce different data classes into `messydt` class
#' @param x A scalar or vector of a class that can be coerced into a Date,
#' such as `Date`, `POSIXct`, `POSIXlt`, or character.
#' @return A `messydt` class object
#' @examples
#' as_messydate("28 BC")
#' as_messydate("2021")
#' as_messydate("2021-02")
#' as_messydate("2021-02-01")
#' as_messydate("20-01-2021")
#' as_messydate("01-20-2021")
#' as_messydate("2021-02-01?")
#' as_messydate("2021-02-01~")
#' as_messydate("2021-02-01%")
#' as_messydate("2021-02-01..2021-02-28")
#' as_messydate("{2021-02-01,2021-02-28}")
#' @export
as_messydate <- function(x) UseMethod("as_messydate")

#' @describeIn as_messydate Coerce from `Date` to `messydt` class
#' @export
as_messydate.Date <- function(x) {
  x <- as.character(x)
  new_messydate(x)
}

#' @describeIn as_messydate Coerce from `POSIXct` to `messydt` class
#' @export
as_messydate.POSIXct <- function(x) {
  x <- as.character(x)
  new_messydate(x)
}

#' @describeIn as_messydate Coerce from `POSIXlt` to `messydt` class
#' @export
as_messydate.POSIXlt <- function(x) {
  x <- as.character(x)
  new_messydate(x)
}

#' @describeIn as_messydate Coerce character date objects to `messydt` class
#' @export
as_messydate.character <- function(x) {
  d <- standardise_date_separators(x)
  d <- standardise_date_order(d)
  d <- standardise_date_input(d)
  d <- standardise_unspecifieds(d)
  d <- standardise_widths(d)
  d <- standardise_ranges(d)
  d <- remove_imprecision(d)
  new_messydate(d)
}

# Helper functions
standardise_date_separators <- function(dates) {
  dates <- stringr::str_replace_all(dates,
                                    "([:digit:]{4})([:digit:]{2})([:digit:]{2})",
                                    "\\1-\\2-\\3")
  dates <- stringr::str_replace_all(dates,
                                    "(?<=[:digit:])\\.(?=[:digit:])",
                                    "-")
  dates <- stringr::str_replace_all(dates, "\\/", "-")
  dates <- stringr::str_trim(dates, side = "both")
  dates
}

standardise_date_order <- function(dates) {
  dates <- ifelse(stringr::str_detect(dates, "^([:digit:]{2})-([:digit:]{2})-([:digit:]{4}$)") &
                    as.numeric(gsub("-", "", stringr::str_extract(dates, "-[:digit:]{2}-"))) > 12,
                  stringr::str_replace_all(dates,
                                           "^([:digit:]{2})-([:digit:]{2})-([:digit:]{4}$)",
                                           "\\3-\\1-\\2"),
                  stringr::str_replace_all(dates,
                                           "^([:digit:]{2})-([:digit:]{2})-([:digit:]{4}$)",
                                           "\\3-\\2-\\1"))
  dates
}

standardise_date_input <- function(dates) {
  as_bc_dates <- function(dates) {
    # remove before christ letters
    dates <- stringr::str_remove_all(dates, "(bc|BC|Bc|bC)")
    dates <- stringr::str_trim(dates, side = "both")
    dates <- paste0("-", dates)
    dates
  }
  as_ac_dates <- function(dates) {
    # remove after christ letters
    dates <- stringr::str_remove_all(dates, "(ad|AD|Ad|aD)")
    dates <- stringr::str_trim(dates, side = "both")
  }
  dates <- ifelse(stringr::str_detect(dates, "(bc|BC|Bc|bC)"),
                  as_bc_dates(dates), dates)
  dates <- ifelse(stringr::str_detect(dates, "(ad|AD|Ad|aD)"),
                  as_ac_dates(dates), dates)
  dates
}

standardise_unspecifieds <- function(dates) {
  dates <- stringr::str_replace_all(dates, "^NA", "XXXX")
  dates <- stringr::str_replace_all(dates, "-NA", "-XX")
  dates <- stringr::str_replace_all(dates, "0000", "XXXX")
  dates <- stringr::str_replace_all(dates, "-00-|-0-|-0$|-00$", "-XX-")
  dates <- stringr::str_replace_all(dates, "\\?\\?\\?\\?", "XXXX")
  dates <- stringr::str_replace_all(dates, "-\\?\\?", "-XX")
  dates
}

standardise_widths <- function(dates) {
  # Negative year only
  dates <- stringr::str_replace_all(dates, "^-([:digit:]{1})$", "-000\\1")
  dates <- stringr::str_replace_all(dates, "^-([:digit:]{2})$", "-00\\1")
  dates <- stringr::str_replace_all(dates, "^-([:digit:]{3})$", "-0\\1")
  # Uncertain and aproximate year only
  dates <- stringr::str_replace_all(dates, "^~([:digit:]{1})$", "000\\1~")
  dates <- stringr::str_replace_all(dates, "^~([:digit:]{2})$", "00\\1~")
  dates <- stringr::str_replace_all(dates, "^~([:digit:]{3})$", "0\\1~")
  dates <- stringr::str_replace_all(dates, "^\\?([:digit:]{1})$", "000\\1?")
  dates <- stringr::str_replace_all(dates, "^\\?([:digit:]{2})$", "00\\1?")
  dates <- stringr::str_replace_all(dates, "^\\?([:digit:]{3})$", "0\\1?")
  dates <- ifelse(stringr::str_detect(dates, "^([:digit:]{1})~$|^([:digit:]{1})\\?$"),
                  paste0("000", dates), dates)
  dates <- ifelse(stringr::str_detect(dates, "^([:digit:]{2})~$|^([:digit:]{2})\\?$"),
                  paste0("00", dates), dates)
  dates <- ifelse(stringr::str_detect(dates, "^([:digit:]{3})~$|^([:digit:]{3})\\?$"),
                  paste0("0", dates), dates)
  # Year only
  dates <- stringr::str_replace_all(dates, "^([:digit:]{1})$", "000\\1")
  dates <- stringr::str_replace_all(dates, "^([:digit:]{2})$", "00\\1")
  dates <- stringr::str_replace_all(dates, "^([:digit:]{3})$", "0\\1")
  # Adds zero padding to months and days
  dates <- stringr::str_replace_all(dates, "-([:digit:])$", "-0\\1")
  dates <- stringr::str_replace_all(dates, "-([:digit:])-", "-0\\1-")
  dates <- stringr::str_replace_all(dates, "^([:digit:])-", "0\\1-")
  dates
}

standardise_ranges <- function(dates) {
  dates <- stringr::str_replace_all(dates, "_", "..")
  dates <- stringr::str_replace_all(dates, ":", "..")
  dates
}

remove_imprecision <- function(dates) {
  dates <- stringr::str_replace_all(dates, "-XX$", "")
  dates
}
