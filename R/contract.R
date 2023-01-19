#' Contract lists of dates into messy dates
#'
#' This function operates as the opposite of `expand()`.
#' It contracts a list of dates into the abbreviated annotation
#' of messy dates.
#' @details The ´contract()´ function first `expand()` 'mdate' objects
#' to then display their most succinct representation.
#' @param x A list of dates
#' @param collapse Do you want ranges to be collapsed?
#' TRUE by default.
#' If FALSE ranges are returned in compact format.
#' @return A `mdate` vector
#' @importFrom tibble tibble
#' @importFrom lubridate NA_Date_
#' @importFrom stringr str_replace_all str_detect str_count str_replace str_extract
#' @importFrom dplyr lead last first
#' @examples
#' d <- as_messydate(c("2001-01-01", "2001-01", "2001",
#' "2001-01-01..2001-02-02", "{2001-10-01,2001-10-04}",
#' "{2001-01,2001-02-02}", "28 BC", "-2000-01-01",
#' "{2001-01-01, 2001-01-02, 2001-01-03}"))
#' tibble::tibble(d, contract(d))
#' @export
contract <- function(x, collapse = TRUE) {
  if (class(x) != "list") {
    x <- expand(x)
  }
  x <- compact_negative_dates(x)
  x <- compact_ranges(x)
  x <- collapse_sets(x)
  if (collapse == TRUE) {
    x <- collapse_ranges(x)
  } else {
    x <- unlist(x)
  }
  as_messydate(x)
}

compact_negative_dates <- function(x) {
  lapply(x, function(d) {
    if (stringr::str_detect(d[1], "^-") & length(d) > 1) {
      d <- paste0(dplyr::first(d), "..", dplyr::last(d))
    }
    d
  })
}

compact_ranges <- function(x) {
  lapply(x, function(d) {
    if (length(d) > 1) {
      sequ <- is_sequence(d)
      if (any(sequ)) {
        starts <- d[which(sequ == FALSE)]
        ends <- d[dplyr::lead(sequ) == FALSE | is.na(dplyr::lead(sequ))]
        if (any(starts == ends)) ends[starts == ends] <- NA
        d <- paste(starts, ends, sep = "..")
        d <- stringr::str_replace_all(d, "\\.\\.NA", "")
      }
    }
    d
  })
}

collapse_sets <- function(x) {
  x <- lapply(x, paste, collapse = ",")
  x <- ifelse(stringr::str_count(x, ",") == 11 &
                stringr::str_detect(x, "-01-") &
                stringr::str_detect(x, "-12-"),
              stringr::str_replace(stringr::str_extract(x, "[^,]*"),
                                   "-01-", "-XX-"), x)
  x <- ifelse(stringr::str_detect(x, ","), paste0("{", x, "}"), x)
  x
}

collapse_ranges <- function(x) {
  x <- stringr::str_replace_all(x, "([:digit:]{4})-01-01\\.\\.([:digit:]{4})-12-31", "\\1")
  x <- stringr::str_replace_all(x, "([:digit:]{4}-[:digit:]{2})-01\\.\\.([:digit:]{4}-[:digit:]{2})-28", "\\1")
  x <- stringr::str_replace_all(x, "([:digit:]{4}-[:digit:]{2})-01\\.\\.([:digit:]{4}-[:digit:]{2})-29", "\\1")
  x <- stringr::str_replace_all(x, "([:digit:]{4}-[:digit:]{2})-01\\.\\.([:digit:]{4}-[:digit:]{2})-30", "\\1")
  x <- stringr::str_replace_all(x, "([:digit:]{4}-[:digit:]{2})-01\\.\\.([:digit:]{4}-[:digit:]{2})-31", "\\1")
  x <- stringr::str_replace_all(x, "(-[:digit:]{4})-01-01\\.\\.(-[:digit:]{4})-12-31", "\\1")
  x <- stringr::str_replace_all(x, "(-[:digit:]{3})-01-01\\.\\.(-[:digit:]{3})-12-31", "\\1")
  x <- stringr::str_replace_all(x, "(-[:digit:]{4}-[:digit:]{2})-01\\.\\.(-[:digit:]{4}-[:digit:]{2})-28", "\\1")
  x <- stringr::str_replace_all(x, "(-[:digit:]{4}-[:digit:]{2})-01\\.\\.(-[:digit:]{4}-[:digit:]{2})-29", "\\1")
  x <- stringr::str_replace_all(x, "(-[:digit:]{4}-[:digit:]{2})-01\\.\\.(-[:digit:]{4}-[:digit:]{2})-30", "\\1")
  x <- stringr::str_replace_all(x, "(-[:digit:]{4}-[:digit:]{2})-01\\.\\.(-[:digit:]{4}-[:digit:]{2})-31", "\\1")
}

is_sequence <- function(x) {
  l <- as.Date(x) + 1
  l <- c(lubridate::NA_Date_, l[-length(l)])
  l <- x == l
  l[is.na(l)] <- FALSE
  l
}
