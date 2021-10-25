#' Expand messy dates to lists of dates
#'
#' These functions expand on unspecified dates, ranges and on sets of dates.
#' Uncertain dates may include several possible dates.
#' The function "opens" these values to include all the possible dates
#' contained in uncertain dates.
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
#' "{2001-01,2001-02-02}", "2008-XX-31"))
#' expand(d)
#' @export
expand.messydt <- function(x) {

  x <- remove_qualifiers(x)
  x <- expand_range(x)
  #x <- expand_unspecified(x)
  #x <- expand_sets(x)
  #x <- expand_ranges(x)

  x
}

remove_qualifiers <- function(dates) {
  dates <- stringr::str_replace_all(dates, "\\~", "\\?")
  # dates <- stringr::str_replace_all(dates, "\\%", "\\?")
  dates <- stringr::str_remove_all(dates, "[:space:]")
  dates <- stringr::str_remove_all(dates, "\\{")
  dates <- stringr::str_replace_all(dates, "\\}", "\\?")
  dates <- ifelse(stringr::str_detect(dates, "-XX-"), paste0(dates, "?"), dates)
  dates
}

expand_range <- function(dates) {

  dates <- stringr::str_split(dates, "\\.\\.")

  dates <- lapply(dates, function(x) {

    # expand range of dates
    if (length(x) == 2) {
      x <- as.character(seq(as.Date(x[1]), as.Date(x[2]), by = "days"))
      x
    }


    else {

      if(stringr::str_detect(x, "\\?") == TRUE) {

        # expand uncertain dates
        x <- stringr::str_remove_all(x, "\\?")

        if (stringr::str_detect(x, "^([:digit:]{4})-([:digit:]{2})-([:digit:]{2})$") == TRUE) {
          b <- as.character(seq(from = as.Date(x), by = "-1 day", length.out = 4))
          a <- as.character(seq(from = as.Date(x), by = "days", length.out = 4))
          x <- c(b, a)
          x <- sort(unique(x))
        }
        else{
          if (stringr::str_detect(x, "^([:digit:]{4})-([:digit:]{2})$") == TRUE) {
            x <- paste0(x, "-28")
            b <- as.character(seq(from = as.Date(x), by = "-1 month", length.out = 4))
            a <- as.character(seq(from = as.Date(x), by = "months", length.out = 4))
            x <- c(b, a)
            x <- stringr::str_remove(x, "-28")
            x <- sort(unique(x))
          }
          else{
            if(stringr::str_detect(x, "(^|,)([:digit:]{4})($|,)") == TRUE) {
              x <- paste0(x, "-01-28")
              b <- as.character(seq(from = as.Date(x), by = "-1 year", length.out = 4))
              a <- as.character(seq(from = as.Date(x), by = "years", length.out = 4))
              x <- c(b, a)
              x <- stringr::str_remove(x, "-01-28")
              x <- sort(unique(x))
            }
            else {
              if(stringr::str_detect(x, "^([:digit:]{4})-XX-([:digit:]{2})$") == TRUE) {
                x <- stringr::str_replace_all(x, "XX", "05")
                b <- as.character(seq(from = as.Date(x), by = "-1 day", length.out = 4))
                a <- as.character(seq(from = as.Date(x), by = "days", length.out = 4))
                x <- c(b, a)
                x <- stringr::str_replace_all(x, "-([:digit:]{2})-", "-XX-")
                x <- sort(unique(x))
              }
              else {
                # expand sets of certain dates
                x <- stringr::str_split(x, ",")
              }
            }
          }
        }
        unlist(x)
      }
      else {
        x
      }
      }
  })
  dates
}
