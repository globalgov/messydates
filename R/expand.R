#' Expand messy dates to lists of dates
#'
#' These functions expand on unspecified dates, ranges and on sets of dates.
#' Uncertain dates may include several possible dates.
#' The function "opens" these values to include all the possible dates
#' contained in uncertain dates.
#' @param x A `messydt` object
#' @export
expand <- function(x) UseMethod("expand")

#' @describeIn expand Expanding messydates
#' @importFrom stringr str_replace_all str_split
#' @examples
#' d <- as_messydate(c("2001-01-01", "2001-01", "2001", "2001-01-01..2001-02-02",
#'         "{2001-01-01,2001-02-02}", "{2001-01,2001-02-02}"))
#' expand(d)
#' @export
expand.messydt <- function(x) {

  x <- remove_qualifiers(x)

  x <- expand_unspecified(x)
  x <- expand_sets(x)
  x <- expand_ranges(x)

  x
}

remove_qualifiers <- function(dates) {
  dates <- stringr::str_remove_all(dates, "\\?")
  dates <- stringr::str_remove_all(dates, "\\~")
  dates <- stringr::str_remove_all(dates, "\\%")
  dates <- stringr::str_remove_all(dates, "[:space:]")
  dates <- stringr::str_remove_all(dates, "\\{")
  dates <- stringr::str_remove_all(dates, "\\}")
  dates
}

expand_unspecified <- function(dates) {
  dates <- stringr::str_replace_all(dates,
                                    "(^|,)([:digit:]{2})XX($|,)", "\\1\\200-01-01..\\299-12-31\\3")
  dates <- stringr::str_replace_all(dates,
                                    "(^|,)([:digit:]{3})X($|,)", "\\1\\20-01-01..\\29-12-31\\3")
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
  dates <- stringr::str_remove_all(dates, "[:space:]")
  dates <- stringr::str_remove_all(dates, "\\{")
  dates <- stringr::str_remove_all(dates, "\\}")
  dates <- stringr::str_split(dates, ",")
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
