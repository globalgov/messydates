#' Coercion to messy dates
#'
#' These functions coerce different data classes into `messydt` class
#' @param x A scalar or vector of a class that can be coerced into a Date,
#' such as `Date`, `POSIXct`, `POSIXlt`, or character.
#' @param text Would you like to extract dates from text?
#' By default false.
#' If TRUE, dates are extracted from text.
#' If multiple dates are identified, only the first date is returned.
#' @param interactive Would you like to choose the order for ambiguous dates?
#' By default FALSE.
#' If TRUE, it allows users to choose the correct order
#' for ambiguous 6 digit dates.
#' @importFrom utils menu
#' @return A `messydt` class object
#' @examples
#' as_messydate("28 BC")
#' as_messydate("2021")
#' as_messydate("2021-02")
#' as_messydate("2021-02-01")
#' as_messydate("20-01-2021")
#' as_messydate("01-20-2021")
#' as_messydate("20 September 2021")
#' as_messydate("2021-02-01?")
#' as_messydate("2021-02-01~")
#' as_messydate("2021-02-01%")
#' as_messydate("2021-02-01..2021-02-28")
#' as_messydate("{2021-02-01,2021-02-28}")
#' @export
as_messydate <- function(x, text, interactive) UseMethod("as_messydate")

#' @describeIn as_messydate Coerce from `Date` to `messydt` class
#' @export
as_messydate.Date <- function(x, text, interactive) {
  x <- as.character(x)
  new_messydate(x)
}

#' @describeIn as_messydate Coerce from `POSIXct` to `messydt` class
#' @export
as_messydate.POSIXct <- function(x, text, interactive) {
  x <- as.character(x)
  new_messydate(x)
}

#' @describeIn as_messydate Coerce from `POSIXlt` to `messydt` class
#' @export
as_messydate.POSIXlt <- function(x, text, interactive) {
  x <- as.character(x)
  new_messydate(x)
}

#' @describeIn as_messydate Coerce character date objects to `messydt` class
#' @export
as_messydate.character <- function(x, text = FALSE, interactive = FALSE) {
  if (isTRUE(text)) {
    x <- extract_from_text(x)
  }
  d <- standardise_date_separators(x)
  d <- standardise_months(d)
  d <- standardise_date_order(d)
  if (isTRUE(interactive)) {
    d <- ask_user(d)
  }
  d <- standardise_ranges(d)
  d <- standardise_unspecifieds(d)
  d <- remove_imprecision(d)
  d <- standardise_date_input(d)
  d <- standardise_widths(d)
  new_messydate(d)
}

# Helper functions
standardise_date_separators <- function(dates) {
  dates <- stringr::str_replace_all(dates,
                                    "(?<=[:digit:])\\.(?=[:digit:])", "-")
  dates <- stringr::str_replace_all(dates, "\\/", "-")
  dates <- stringr::str_remove_all(dates, "\\(|\\)|\\{|\\}|\\[|\\]")
  dates <- stringr::str_trim(dates, side = "both")
  dates
}

standardise_months <- function(dates) {
  dates <- gsub(" january ", "-01-", dates, ignore.case = TRUE)
  dates <- gsub(" february ", "-02-", dates, ignore.case = TRUE)
  dates <- gsub(" march ", "-03-", dates, ignore.case = TRUE)
  dates <- gsub(" april ", "-04-", dates, ignore.case = TRUE)
  dates <- gsub(" may ", "-05-", dates, ignore.case = TRUE)
  dates <- gsub(" june ", "-06-", dates, ignore.case = TRUE)
  dates <- gsub(" july ", "-07-", dates, ignore.case = TRUE)
  dates <- gsub(" august ", "-08-", dates, ignore.case = TRUE)
  dates <- gsub(" september ", "-09-", dates, ignore.case = TRUE)
  dates <- gsub(" october ", "-10-", dates, ignore.case = TRUE)
  dates <- gsub(" november ", "-11-", dates, ignore.case = TRUE)
  dates <- gsub(" december ", "-12-", dates, ignore.case = TRUE)
  dates <- stringr::str_squish(dates)
  dates <- stringr::str_replace_all(dates, "-([:digit:])-", "-0\\1-")
  dates
}

standardise_date_order <- function(dates) {
  dates <- ifelse(stringr::str_detect(dates, "^([:digit:]{2})-([:digit:]{2})-([:digit:]{4}$)") &
                    as.numeric(gsub("-", "", stringr::str_extract(dates, "-[:digit:]{2}-"))) > 12,
                  stringr::str_replace_all(dates, "^([:digit:]{2})-([:digit:]{2})-([:digit:]{4}$)",
                                           "\\3-\\1-\\2"),
                  stringr::str_replace_all(dates, "^([:digit:]{2})-([:digit:]{2})-([:digit:]{4}$)",
                                           "\\3-\\2-\\1"))
  dates <- ifelse(stringr::str_detect(dates, "^([:digit:]{2})-([:digit:]{2})-([:digit:]{2}$)") &
                    as.numeric(gsub("-", "", stringr::str_extract(dates, "-[:digit:]{2}-"))) < 12 &
                    as.numeric(gsub("-", "", stringr::str_extract(dates, "[:digit:]{2}$"))) > 31,
                  stringr::str_replace_all(dates,
                           "^([:digit:]{2})-([:digit:]{2})-([:digit:]{2}$)",
                           "\\3-\\2-\\1"), dates)
  dates <- ifelse(stringr::str_detect(dates, "^([:digit:]{2})-([:digit:]{2})-([:digit:]{2}$)") &
                    as.numeric(gsub("-", "", stringr::str_extract(dates, "-[:digit:]{2}-"))) > 12 &
                    as.numeric(gsub("-", "", stringr::str_extract(dates, "[:digit:]{2}$"))) > 31,
                  stringr::str_replace_all(dates, "^([:digit:]{2})-([:digit:]{2})-([:digit:]{2}$)",
                                           "\\3-\\1-\\2"), dates)
  dates
}

standardise_ranges <- function(dates) {
  dates <- stringr::str_replace_all(dates, "_", "..")
  dates <- stringr::str_replace_all(dates, ":", "..")
  dates
}

standardise_unspecifieds <- function(dates) {
  dates <- stringr::str_replace_all(dates, "^NA", "XXXX")
  dates <- stringr::str_replace_all(dates, "-NA", "-XX")
  dates <- stringr::str_replace_all(dates, "0000", "XXXX")
  dates <- stringr::str_replace_all(dates, "-00-|-0-|-0$|-00$", "-XX-")
  dates <- stringr::str_replace_all(dates, "\\?\\?\\?\\?", "XXXX")
  dates <- stringr::str_replace_all(dates, "-\\?\\?", "-XX")
  dates <- ifelse(stringr::str_detect(dates, "^[:digit:]{4}\\~$"),
                  paste0("~", stringr::str_remove(dates, "\\~")), dates)
  dates
}

remove_imprecision <- function(dates) {
  dates <- stringr::str_replace_all(dates, "-XX$", "")
  dates
}

standardise_date_input <- function(dates) {
  dates <- ifelse(stringr::str_detect(dates, "(bc|BC|Bc|bC)"),
                  as_bc_dates(dates), dates)
  dates <- ifelse(stringr::str_detect(dates, "(ad|AD|Ad|aD)"),
                  as_ac_dates(dates), dates)
  dates <- gsub(" ", "", dates)
  dates
}

standardise_widths <- function(dates) {
  dates <- add_zero_padding(dates)
  dates <- ifelse(stringr::str_detect(dates, "([:digit:]{1})\\.\\.([:digit:]{1})|([:digit:]{1})\\.\\.-"),
                  add_zero_range(dates), dates)
  dates <- ifelse(stringr::str_detect(dates, "\\,"),
                  add_zero_set(dates), dates)
  dates <- ifelse(stringr::str_detect(dates, "\\,"),
                  paste0("{", dates, "}"), dates)
  dates
}

add_zero_padding <- function(dates) {
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
  dates <- ifelse(stringr::str_detect(dates,
                                      "^([:digit:]{1})~$|^([:digit:]{1})\\?$"),
                  paste0("000", dates), dates)
  dates <- ifelse(stringr::str_detect(dates,
                                      "^([:digit:]{2})~$|^([:digit:]{2})\\?$"),
                  paste0("00", dates), dates)
  dates <- ifelse(stringr::str_detect(dates,
                                      "^([:digit:]{3})~$|^([:digit:]{3})\\?$"),
                  paste0("0", dates), dates)
  # Year only
  dates <- stringr::str_replace_all(dates, "^([:digit:]{1})$", "000\\1")
  dates <- stringr::str_replace_all(dates, "^([:digit:]{2})$", "00\\1")
  dates <- stringr::str_replace_all(dates, "^([:digit:]{3})$", "0\\1")
  # Adds zero padding to days
  dates <- stringr::str_replace_all(dates, "-([:digit:])$", "-0\\1")
  dates <- stringr::str_replace_all(dates, "^([:digit:])-", "0\\1-")
  dates
}

add_zero_range <- function(dates) {
  dates <- strsplit(dates, "\\.\\.")
  dates <- lapply(dates, function(x) {
    x <- gsub(" ", "", x)
    x <- stringr::str_replace_all(x, "^-([:digit:]{1})$", "-000\\1")
    x <- stringr::str_replace_all(x, "^-([:digit:]{2})$", "-00\\1")
    x <- stringr::str_replace_all(x, "^-([:digit:]{3})$", "-0\\1")
    x <- stringr::str_replace_all(x, "^~([:digit:]{1})$", "000\\1~")
    x <- stringr::str_replace_all(x, "^~([:digit:]{2})$", "00\\1~")
    x <- stringr::str_replace_all(x, "^~([:digit:]{3})$", "0\\1~")
    x <- stringr::str_replace_all(x, "^\\?([:digit:]{1})$", "000\\1?")
    x <- stringr::str_replace_all(x, "^\\?([:digit:]{2})$", "00\\1?")
    x <- stringr::str_replace_all(x, "^\\?([:digit:]{3})$", "0\\1?")
    x <- stringr::str_replace_all(x, "^([:digit:]{1})$", "000\\1")
    x <- stringr::str_replace_all(x, "^([:digit:]{2})$", "00\\1")
    x <- stringr::str_replace_all(x, "^([:digit:]{3})$", "0\\1")
    x <- ifelse(stringr::str_detect(x, "^([:digit:]{1})~$|^([:digit:]{1})\\?$"),
                paste0("000", x), x)
    x <- ifelse(stringr::str_detect(x, "^([:digit:]{2})~$|^([:digit:]{2})\\?$"),
                paste0("00", x), x)
    x <- ifelse(stringr::str_detect(x, "^([:digit:]{3})~$|^([:digit:]{3})\\?$"),
                paste0("0", x), x)
  })
  dates <- purrr::map_chr(dates, paste, collapse = "..")
  dates
}

add_zero_set <- function(dates) {
  dates <- strsplit(dates, "\\,")
  dates <- lapply(dates, function(x) {
    x <- gsub(" ", "", x)
    x <- stringr::str_replace_all(x, "^-([:digit:]{1})$", "-000\\1")
    x <- stringr::str_replace_all(x, "^-([:digit:]{2})$", "-00\\1")
    x <- stringr::str_replace_all(x, "^-([:digit:]{3})$", "-0\\1")
    x <- stringr::str_replace_all(x, "^~([:digit:]{1})$", "000\\1~")
    x <- stringr::str_replace_all(x, "^~([:digit:]{2})$", "00\\1~")
    x <- stringr::str_replace_all(x, "^~([:digit:]{3})$", "0\\1~")
    x <- stringr::str_replace_all(x, "^\\?([:digit:]{1})$", "000\\1?")
    x <- stringr::str_replace_all(x, "^\\?([:digit:]{2})$", "00\\1?")
    x <- stringr::str_replace_all(x, "^\\?([:digit:]{3})$", "0\\1?")
    x <- stringr::str_replace_all(x, "^([:digit:]{1})$", "000\\1")
    x <- stringr::str_replace_all(x, "^([:digit:]{2})$", "00\\1")
    x <- stringr::str_replace_all(x, "^([:digit:]{3})$", "0\\1")
    x <- ifelse(stringr::str_detect(x, "^([:digit:]{1})~$|^([:digit:]{1})\\?$"),
                paste0("000", x), x)
    x <- ifelse(stringr::str_detect(x, "^([:digit:]{2})~$|^([:digit:]{2})\\?$"),
                paste0("00", x), x)
    x <- ifelse(stringr::str_detect(x, "^([:digit:]{3})~$|^([:digit:]{3})\\?$"),
                paste0("0", x), x)
  })
  dates <- purrr::map_chr(dates, paste, collapse = ",")
  dates
}

as_bc_dates <- function(dates) {
  dates <- ifelse(stringr::str_count(dates, "(bc|BC|Bc|bC)") == 2,
                  st_negative_range(dates), dates)
  dates <- ifelse(stringr::str_count(dates, "(bc|BC|Bc|bC)") > 2,
                  st_negative_sets(dates), dates)
  dates <- ifelse(stringr::str_count(dates, "(bc|BC|Bc|bC)") == 1,
                  st_negative(dates), dates)
}

as_ac_dates <- function(dates) {
  # remove after christ letters
  dates <- stringr::str_remove_all(dates, "(ad|AD|Ad|aD)")
  dates <- stringr::str_trim(dates, side = "both")
}

st_negative_range <- function(dates) {
  dates <- stringr::str_remove_all(dates, "(bc|BC|Bc|bC)")
  dates <- gsub(" ", "", dates)
  dates <- paste0("-", strsplit(dates, "\\.\\.")[[1]][1],
                  "..-", strsplit(dates, "\\.\\.")[[1]][2])
}

st_negative_sets <- function(dates) {
  dates <- stringr::str_remove_all(dates, "(bc|BC|Bc|bC)")
  dates <- gsub(" ", "", dates)
  dates <- unlist(strsplit(dates, "\\,"))
  dates <- ifelse(length(dates) > 1,
                  paste0("-", paste(dates, collapse = ", -")),
                  paste0("-", dates))
}

st_negative <- function(dates) {
  dates <- stringr::str_remove_all(dates, "(bc|BC|Bc|bC)")
  dates <- stringr::str_trim(dates, side = "both")
  dates <- paste0("-", dates)
}

extract_from_text <- function(v) {
  # make all lower case
  out <- tolower(v)
  # remove commas
  out <- gsub(",", "", out)
  # remove ordinal signs and date related articles
  out <- gsub("de |of ", " ", out)
  out <- ifelse(grepl("\\dst |\\dnd |\\drd |\\dth |\\d day ", out),
                gsub("st |nd |rd |th |day ", " ", out), out)
  # add hyphen between numbers in words
  out <- ifelse(grepl("\\w*ty\\b \\w*st\\b|\\w*ty\\b \\w*nd\\b|
                      |\\w*ty\\b \\w*rd\\b|\\w*ty\\b \\w*th\\b", out),
                gsub("ty ", "ty-", out), out)
  # correct double white space left
  out <- stringr::str_squish(out)
  # get the first date per row
  out <- stringr::str_extract(out,
                              "[:digit:]{2}\\s[:alpha:]{3}\\s[:digit:]{4}|
                              |[:digit:]{2}\\s[:alpha:]{4}\\s[:digit:]{4}|
                              |[:digit:]{2}\\s[:alpha:]{5}\\s[:digit:]{4}|
                              |[:digit:]{2}\\s[:alpha:]{6}\\s[:digit:]{4}|
                              |[:digit:]{2}\\s[:alpha:]{7}\\s[:digit:]{4}|
                              |[:digit:]{2}\\s[:alpha:]{8}\\s[:digit:]{4}|
                              |[:digit:]{2}\\s[:alpha:]{9}\\s[:digit:]{4}|
                              |[:digit:]{1}\\s[:alpha:]{3}\\s[:digit:]{4}|
                              |[:digit:]{1}\\s[:alpha:]{4}\\s[:digit:]{4}|
                              |[:digit:]{1}\\s[:alpha:]{5}\\s[:digit:]{4}|
                              |[:digit:]{1}\\s[:alpha:]{6}\\s[:digit:]{4}|
                              |[:digit:]{1}\\s[:alpha:]{7}\\s[:digit:]{4}|
                              |[:digit:]{1}\\s[:alpha:]{8}\\s[:digit:]{4}|
                              |[:digit:]{2}\\s[:alpha:]{9}\\s[:digit:]{4}|
                              |[:alpha:]{3}\\s[:digit:]{2}\\s[:digit:]{4}|
                              |[:alpha:]{4}\\s[:digit:]{2}\\s[:digit:]{4}|
                              |[:alpha:]{5}\\s[:digit:]{2}\\s[:digit:]{4}|
                              |[:alpha:]{6}\\s[:digit:]{2}\\s[:digit:]{4}|
                              |[:alpha:]{7}\\s[:digit:]{2}\\s[:digit:]{4}|
                              |[:alpha:]{8}\\s[:digit:]{2}\\s[:digit:]{4}|
                              |[:alpha:]{9}\\s[:digit:]{2}\\s[:digit:]{4}|
                              |[:alpha:]{3}\\s[:digit:]{1}\\s[:digit:]{4}|
                              |[:alpha:]{4}\\s[:digit:]{1}\\s[:digit:]{4}|
                              |[:alpha:]{5}\\s[:digit:]{1}\\s[:digit:]{4}|
                              |[:alpha:]{6}\\s[:digit:]{1}\\s[:digit:]{4}|
                              |[:alpha:]{7}\\s[:digit:]{1}\\s[:digit:]{4}|
                              |[:alpha:]{8}\\s[:digit:]{1}\\s[:digit:]{4}|
                              |[:alpha:]{9}\\s[:digit:]{1}\\s[:digit:]{4}|
                              |[:digit:]{2}-[:digit:]{2}-[:digit:]{4}|
                              |[:digit:]{1}-[:digit:]{2}-[:digit:]{4}|
                              |[:digit:]{2}-[:digit:]{2}-[:digit:]{2}|
                              |[:digit:]{1}-[:digit:]{2}-[:digit:]{2}|
                              |[:digit:]{2}-[:digit:]{1}-[:digit:]{4}|
                              |[:digit:]{1}-[:digit:]{1}-[:digit:]{4}|
                              |[:digit:]{2}-[:digit:]{1}-[:digit:]{2}|
                              |[:digit:]{1}-[:digit:]{1}-[:digit:]{2}|
                              |[:digit:]{4}-[:digit:]{2}-[:digit:]{2}|
                              |[:digit:]{4}-[:digit:]{2}-[:digit:]{1}|
                              |[:digit:]{4}-[:digit:]{1}-[:digit:]{2}|
                              |[:digit:]{4}-[:digit:]{1}-[:digit:]{1}|
                              |[:digit:]{2}/[:digit:]{2}/[:digit:]{4}|
                              |[:digit:]{1}/[:digit:]{2}/[:digit:]{4}|
                              |[:digit:]{2}/[:digit:]{2}/[:digit:]{2}|
                              |[:digit:]{1}/[:digit:]{2}/[:digit:]{2}|
                              |[:digit:]{2}/[:digit:]{1}/[:digit:]{4}|
                              |[:digit:]{1}/[:digit:]{1}/[:digit:]{4}|
                              |[:digit:]{2}/[:digit:]{1}/[:digit:]{2}|
                              |[:digit:]{1}/[:digit:]{1}/[:digit:]{2}|
                              |[:digit:]{4}/[:digit:]{2}/[:digit:]{2}|
                              |[:digit:]{4}/[:digit:]{2}/[:digit:]{1}|
                              |[:digit:]{4}/[:digit:]{1}/[:digit:]{2}|
                              |[:digit:]{4}/[:digit:]{1}/[:digit:]{1}|
                              |[:digit:]{3}\\s[:alpha:]{4}\\s[:digit:]{2}|
                              |[:digit:]{4}\\s[:alpha:]{4}\\s[:digit:]{2}|
                              |[:digit:]{4}\\s[:alpha:]{5}\\s[:digit:]{2}|
                              |[:digit:]{4}\\s[:alpha:]{5}\\s[:digit:]{2}|
                              |[:digit:]{4}\\s[:alpha:]{6}\\s[:digit:]{2}|
                              |[:digit:]{4}\\s[:alpha:]{7}\\s[:digit:]{2}|
                              |[:digit:]{4}\\s[:alpha:]{8}\\s[:digit:]{2}|
                              |[:digit:]{4}\\s[:alpha:]{9}\\s[:digit:]{2}|
                              |[:digit:]{3}\\s[:alpha:]{4}\\s[:digit:]{1}|
                              |[:digit:]{4}\\s[:alpha:]{4}\\s[:digit:]{1}|
                              |[:digit:]{4}\\s[:alpha:]{5}\\s[:digit:]{1}|
                              |[:digit:]{4}\\s[:alpha:]{5}\\s[:digit:]{1}|
                              |[:digit:]{4}\\s[:alpha:]{6}\\s[:digit:]{1}|
                              |[:digit:]{4}\\s[:alpha:]{7}\\s[:digit:]{1}|
                              |[:digit:]{4}\\s[:alpha:]{8}\\s[:digit:]{1}|
                              |[:digit:]{4}\\s[:alpha:]{9}\\s[:digit:]{1}|
                              |\\w*st\\b day.*ty$|\\w*st\\b day.*ty-one|
                              |\\w*st\\b day.*ty-two|\\w*st\\b day.*ty-three|
                              |\\w*st\\b day.*ty-four|\\w*st\\b day.*ty-five|
                              |\\w*st\\b day.*ty-six|\\w*st\\b day.*ty-seven|
                              |\\w*st\\b day.*ty-eight|\\w*st\\b day.*ty-nine|
                              |\\w*nd\\b day.*ty$|\\w*nd\\b day.*ty-one|
                              |\\w*nd\\b day.*ty-two|\\w*nd\\b day.*ty-three|
                              |\\w*nd\\b day.*ty-four|\\w*nd\\b day.*ty-five|
                              |\\w*nd\\b day.*ty-six|\\w*nd\\b day.*ty-seven|
                              |\\w*nd\\b day.*ty-eight|\\w*nd\\b day.*ty-nine|
                              |\\w*rd\\b day.*ty$|\\w*rd\\b day.*ty-one|
                              |\\w*rd\\b day.*ty-two|\\w*rd\\b day.*ty-three|
                              |\\w*rd\\b day.*ty-four|\\w*rd\\b day.*ty-five|
                              |\\w*rd\\b day.*ty-six|\\w*rd\\b day.*ty-seven|
                              |\\w*rd\\b day.*ty-eight|\\w*rd\\b day.*ty-nine|
                              |\\w*th\\b day.*ty$|\\w*th\\b day.*ty-one|
                              |\\w*th\\b day.*ty-two|\\w*th\\b day.*ty-three|
                              |\\w*th\\b day.*ty-four|\\w*th\\b day.*ty-five|
                              |\\w*th\\b day.*ty-six|\\w*th\\b day.*ty-seven|
                              |\\w*th\\b day.*ty-eight|\\w*th\\b day.*ty-nine|
                              |\\w*st\\b .*[:digit:]{4}|\\w*nd\\b .*[:digit:]{4}|
                              |\\w*rd\\b .*[:digit:]{4}|\\w*th\\b .*[:digit:]{4}|
                              |\\w*ty-.*st\\b .*[:digit:]{4}|
                              |\\w*ty-.*nd\\b .*[:digit:]{4}|
                              |\\w*ty-.*rd\\b .*[:digit:]{4}|
                              |\\w*ty-.*th\\b .*[:digit:]{4}")
  # remove all 'day' and 'year' from string
  out <- stringr::str_remove_all(out, "day")
  out <- stringr::str_remove_all(out, "year|in the year")
  out <- stringr::str_squish(out)
  # get the days into numeric form
  days <- data.frame(days = c("^first", "^second", "^third", "^fourth",
                              "^fifth", "^sixth", "^seventh", "^eighth",
                              "^ninth", "tenth", "eleventh", "twelfth",
                              "thirteenth", "fourteenth", "fifteenth",
                              "sixteenth", "seventeenth", "eighteenth",
                              "nineteenth", "twentieth", "twenty-first",
                              "twenty-second", "twenty-third", "twenty-fourth",
                              "twenty-fifth", "twenty-sixth", "twenty-seventh",
                              "twenty-eighth", "twenty-ninth", "thirtieth",
                              "thirty-first"),
                     number = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14,
                                15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26,
                                27, 28, 29, 30, 31))
  for (k in seq_len(nrow(days))) {
    out <- gsub(paste0(days$days[k]),
                paste0(days$number[k]),
                out, ignore.case = TRUE,
                perl = T)
  }
  # get the months into numeric form
  months <- data.frame(months = c("january", "february", "march", "april",
                                  "may", "june", "july", "august", "september",
                                  "october", "november", "december"),
                       number = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))
  for (k in seq_len(nrow(months))) {
    out <- gsub(paste0(months$months[k]),
                paste0(months$number[k]),
                out, ignore.case = TRUE,
                perl = T)
  }
  # get the years into numeric form
  year <- stringr::str_extract(out, "one.*|two.*")
  for (k in seq(match_year(out))) {
    out <- gsub(paste0(year[k]),
                paste0(match_year(out)[k]),
                out, ignore.case = TRUE,
                perl = T)
  }
  # standardize separators
  out <- stringr::str_replace_all(out, " |/", "-")
  out <- stringr::str_replace_all(out, "[a-z]|[A-Z]", "?")
  out
}

match_year <- function(string) {
  nums <- list(one = 1, two = 2, three = 3, four = 4, five = 5, six = 6,
               seven = 7, eight = 8, nine = 9, ten = 10, eleven = 11,
               twelve = 12, thirteen = 13, fourteen = 14, fifteen = 15,
               sixteen = 16, seventeen = 17, eighteen = 18, nineteen = 19,
               twenty = 20, thirty = 30, forty = 40, fifty = 50,
               sixty = 60, seventy = 70, eighty = 80, ninety = 90)
  year <- stringr::str_extract(string, "one.*|two.*")
  words <- stringr::str_split(year, " ")
  year.date <- lapply(words, function(x) {
    multi.y1 <- ifelse(x[1] == "one" | x[1] == "two", as.numeric(nums[x[1]]), 0)
    y1 <- ifelse(x[2] == "thousand", 1000, 0)
    multi.y2 <- ifelse(x[3] != "and", as.numeric(nums[x[3]]), 0)
    y2 <- ifelse(x[4] == "hundred", 100, 0)
    y3 <- ifelse(x[5] == "and", paste0(x[6]), 0)
    y3 <- unlist(stringr::str_split(y3, "-"))
    y3a <- y3[1]
    y3a <- unlist(nums[y3a])
    y3b <- y3[2]
    y3b <- unlist(nums[y3b])
    y3 <- sum(y3a, y3b)
    out <- multi.y1 * y1 + multi.y2 * y2 + y3
    out
  })
  year.date
}

ask_user <- function(dates) {
  dates <- ifelse(stringr::str_detect(dates, "^([:digit:]{2})-([:digit:]{2})-([:digit:]{2}$)") &
                    as.numeric(gsub("-", "", stringr::str_extract(dates, "^[:digit:]{2}-"))) < 32 &
                    as.numeric(gsub("-", "", stringr::str_extract(dates, "-[:digit:]{2}-"))) < 32 &
                    as.numeric(gsub("-", "", stringr::str_extract(dates, "[:digit:]{2}$"))) < 32,
                  ask_user_input(dates), dates)
}

ask_user_input <- function(d) {
  input <- menu(c("Year-Month-Day", "Day-Month-Year", "Month-Day-Year"),
                title = paste0("What format is this date ", d, " ?"))
  if (input == 1) {
    out <- d
    message("Dates already in standard YMD format")
  }
  if (input == 2) {
    out <- stringr::str_replace_all(d, "^([:digit:]{2})-([:digit:]{2})-([:digit:]{2})$",
                                    "\\3-\\2-\\1")
    message("Ambiguous 6 digit dates have been changed to standard YMD format")
  }
  if (input == 3) {
    out <- stringr::str_replace_all(d, "^([:digit:]{2})-([:digit:]{2})-([:digit:]{2})$",
                                    "\\3-\\1-\\2")
    message("Ambiguous 6 digit dates have been changed to standard YMD format")
  }
  out
}
