#' Coercion to messy dates
#'
#' These functions coerce different data classes into `messydt` class
#' @param x A scalar or vector of a class that can be coerced into a Date,
#' such as `Date`, `POSIXct`, `POSIXlt`, or character.
#' @param from_text Would you like to extract dates from text?
#' By default false.
#' If TRUE, dates are extracted from text.
#' If multiple dates are identified, only the first date is returned.
#' Currently, this only works for texts in English.
#' @param resequence Would you like to choose the order for ambiguous dates?
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
#' as_messydate("Two of February of two thousand twenty-two",
#' from_text = TRUE)
#' #as_messydate("01-02-21", resequence = TRUE)
#' @export
as_messydate <- function(x, from_text, resequence) UseMethod("as_messydate")

#' @describeIn as_messydate Coerce from `Date` to `messydt` class
#' @export
as_messydate.Date <- function(x, from_text, resequence) {
  x <- as.character(x)
  new_messydate(x)
}

#' @describeIn as_messydate Coerce from `POSIXct` to `messydt` class
#' @export
as_messydate.POSIXct <- function(x, from_text, resequence) {
  x <- as.character(x)
  new_messydate(x)
}

#' @describeIn as_messydate Coerce from `POSIXlt` to `messydt` class
#' @export
as_messydate.POSIXlt <- function(x, from_text, resequence) {
  x <- as.character(x)
  new_messydate(x)
}

#' @describeIn as_messydate Coerce character date objects to `messydt` class
#' @export
as_messydate.character <- function(x, from_text = FALSE, resequence = FALSE) {
  if (isTRUE(from_text)) {
    x <- extract_from_text(x)
  }
  d <- standardise_date_separators(x)
  d <- standardise_months(d)
  d <- standardise_date_order(d)
  if (isTRUE(resequence)) {
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
  # get ordinal and numeric dates spelled and replace in text
  out <- stringr::str_squish(gsub(" of| and | day| year| month", " ", v))
  for (k in seq_len(nrow(text_to_number))) {
    out <- gsub(paste0(text_to_number$text[k]),
                paste0(text_to_number$numeric[k]),
                out, ignore.case = TRUE,
                perl = T)
  }
  # get the months into date form
  months <- data.frame(months = c("january", "february", "march", "april",
                                  "may", "june", "july", "august", "september",
                                  "october", "november", "december"),
                       numeric = c("-01-", "-02-", "-03-", "-04-", "-05-",
                                  "-06-", "-07-", "-08-", "-09-", "-10-",
                                  "-11-", "-12-"))
  for (k in seq_len(nrow(months))) {
    out <- gsub(paste0(months$months[k]),
                paste0(months$numeric[k]),
                out, ignore.case = TRUE,
                perl = T)
  }
  # correct double white space left and standardize separators
  out <- gsub("- -| -|- | /", "-", stringr::str_squish(out))
  # get the first date per row
  out <- stringr::str_extract(out,
                              "[:digit:]{2}-[:digit:]{2}-[:digit:]{4}|
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
                              |[:digit:]{4}-[:digit:]{1}-[:digit:]{1}")
  out
}

ask_user <- function(dates) {
  dates <- ifelse(stringr::str_detect(dates, "^([:digit:]{2})-([:digit:]{2})-([:digit:]{2})$|
  |^([:digit:]{1})-([:digit:]{2})-([:digit:]{2})$|^([:digit:]{1})-([:digit:]{2})-([:digit:]{1})$|
                                      |^([:digit:]{2})-([:digit:]{2})-([:digit:]{1})$") &
                    as.numeric(gsub("-", "", stringr::str_extract(dates, "^[:digit:]{2}-|^[:digit:]{1}-"))) < 32 &
                    as.numeric(gsub("-", "", stringr::str_extract(dates, "-[:digit:]{2}-"))) < 32 &
                    as.numeric(gsub("-", "", stringr::str_extract(dates, "[:digit:]{2}$|[:digit:]{1}$"))) < 32,
                  ask_user_input(dates), dates)
}

ask_user_input <- function(d) {
  input <- menu(c("YMD (Year-Month-Day)", "DMY (Day-Month-Year)",
                  "MDY (Month-Day-Year)"),
                title = paste0("What should the component order of ambiguous 6 digit dates, such as this one ", d, " be?"))
  if (input == 1) {
    out <- d
    message("Ambiguous 6 digit dates already in standard YMD format")
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
