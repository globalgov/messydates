#' Extract dates from text
#'
#' Sometimes dates can be contained in text,
#' this function extracts those dates from text and
#' converts them to messydt objects.
#' @param v A character vector
#' @return Dates in messydt format
#' @importFrom stringr str_replace_all str_squish str_extract word str_starts
#' str_remove_all
#' @details If multiple dates are identified in the same row,
#' only the first date is returned.
#' @examples
#' v <- c("This function was created on the 29 September 2021",
#' "Today is October 12, 2021", "Tomorrow is 13-10-2021")
#' text_to_date(v)
#' @export
text_to_date <- function(v) {
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
  out <- extract_from_text(out)
  # remove all 'day' and 'year' from string
  out <- stringr::str_remove_all(out, "day")
  out <- stringr::str_remove_all(out, "year|in the year")
  out <- stringr::str_squish(out)
  # re-order dates if necessary
  out <- ifelse(stringr::str_count(out, '\\w+') > 3, out,
                lapply(out, re_order))
  # get the days into numeric form
  days <- data.frame(days = c("^first", "^second", "^third", "^fourth", "^fifth",
                              "^sixth", "^seventh", "^eighth", "^ninth", "tenth",
                              "eleventh", "twelfth", "thirteenth", "fourteenth",
                              "fifteenth", "sixteenth", "seventeenth", "eighteenth",
                              "nineteenth", "twentieth", "twenty-first", "twenty-second",
                              "twenty-third", "twenty-fourth", "twenty-fifth",
                              "twenty-sixth", "twenty-seventh", "twenty-eighth",
                              "twenty-ninth", "thirtieth", "thirty-first"),
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
  out <- as_messydate(ifelse(out == "", NA, out))
  out
}

extract_from_text <- function(x) {
  out <- stringr::str_extract(x,
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
                              |\\w*st\\b day.*[:digit:]{4}|\\w*nd\\b day.*[:digit:]{4}|
                              |\\w*rd\\b day.*[:digit:]{4}|\\w*th\\b day.*[:digit:]{4}|
                              |\\w*ty-.*st\\b day.*[:digit:]{4}|
                              |\\w*ty-.*nd\\b day.*[:digit:]{4}|
                              |\\w*ty-.*rd\\b day.*[:digit:]{4}|
                              |\\w*ty-.*th\\b day.*[:digit:]{4}")
  out
}

re_order <- function(l) {
  st <- stringr::word(l, 1)
  mi <- stringr::word(l, 2)
  ed <- stringr::word(l, 3)
  out <- ifelse(stringr::str_starts(l, "[:digit:]{4}"),
                paste0(ed, "-", mi, "-", st), l)
  out <- ifelse(stringr::str_starts(out, "[:alpha:]"),
                paste0(mi, "-", st, "-", ed), out)
  out <- stringr::str_remove_all(out, "-NA|NA-|NA")
  out
}

match_year <- function(string) {
  nums <- list(one=1, two=2, three=3, four=4, five=5,
               six=6, seven=7, eight=8, nine=9, ten=10,
               eleven=11, twelve=12, thirteen=13, fourteen=14, fifteen=15,
               sixteen=16, seventeen=17, eighteen=18, nineteen=19,
               twenty=20, thirty=30, forty=40, fifty=50,
               sixty=60, seventy=70, eighty=80, ninety=90)
  year <- stringr::str_extract(string, "one.*|two.*")
  words <- stringr::str_split(year, " ")
  year.date <- lapply(words, function(x) {
    multi.y1 <- ifelse(x[1] == "one"|x[1] == "two", as.numeric(nums[x[1]]), 0)
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
    out <- multi.y1*y1 + multi.y2*y2 + y3
    out
  })
  year.date
}
