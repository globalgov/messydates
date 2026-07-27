#' Contract lists of dates into messy dates
#' @description
#'   This function operates as the opposite of `expand()`.
#'   It contracts a list of dates into the abbreviated annotation
#'   of messy dates.
#' @name convert_contract
#' @details The ´contract()´ function first `expand()` 'mdate' objects
#' to then display their most succinct representation.
#'
#' Because `expand()` drops the time of day from ranges (see `?expand`),
#' contracting a date-time range and then re-expanding it will not restore
#' the original times; `contract()` is intended for date-level ranges,
#' sets, and unspecified components.
#'
#' Sets are returned in the notation they were given in: a `[]` ("one member
#' of") set contracts back to `[]` rather than `{}`. This is only possible
#' when `contract()` is passed an `mdate` (or something coercible to one),
#' since `expand()` returns the member dates without recording which kind of
#' set they came from. A list of dates therefore always contracts to `{}`.
#' Note that a set whose members happen to be consecutive days contracts to a
#' range, `..`, whichever notation it was given in.
#' @param x A list of dates
#' @param collapse Do you want ranges to be collapsed?
#'   TRUE by default.
#'   If FALSE ranges are returned in compact format.
#' @return A `mdate` vector
#' @importFrom lubridate NA_Date_
#' @examples
#' d <- as_messydate(c("2001-01-01", "2001-01", "2001",
#' "2001-01-01..2001-02-02", "{2001-10-01,2001-10-04}",
#' "{2001-01,2001-02-02}", "28 BC", "-2000-01-01",
#' "{2001-01-01, 2001-01-02, 2001-01-03}"))
#' data.frame(d, contracted = contract(d))
#' # a full-month range collapses to a year-month by default...
#' contract(as_messydate("2012-06-01..2012-06-30"))
#' # ...unless collapse = FALSE keeps it as an explicit start..end range
#' contract(as_messydate("2012-06-01..2012-06-30"), collapse = FALSE)
#' # a '[]' set stays a '[]' set
#' contract(as_messydate("[2001-01-01,2001-02-02]"))
#' @export
contract <- function(x, collapse = TRUE) {
  onesie <- rep(FALSE, length(x))
  if (!inherits(x, 'list')) {
    onesie <- stringi::stri_detect_regex(as.character(x), "^\\[.*\\]$")
    onesie[is.na(onesie)] <- FALSE
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
  x <- restore_onesies(x, onesie)
  as_messydate(x)
}

# Re-emits the '[]' notation for those elements that were given as "one member
# of" sets, since `collapse_sets()` can only know that the members belong
# together, not which kind of set they formed.
restore_onesies <- function(x, onesie) {
  if (!any(onesie)) return(x)
  wrapped <- onesie & stringi::stri_detect_regex(x, "^\\{.*\\}$")
  x[wrapped] <- stringi::stri_replace_all_regex(x[wrapped],
                                                "^\\{(.*)\\}$", "[$1]")
  x
}

compact_negative_dates <- function(x) {
  lapply(x, function(d) {
    if (stringi::stri_detect_regex(d[1], "^-") & length(d) > 1) {
      d <- paste0(d[1], "..", d[length(d)])
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
        led <- c(sequ[-1], NA)
        ends <- d[led == FALSE | is.na(led)]
        if (any(starts == ends)) ends[starts == ends] <- NA
        d <- paste(starts, ends, sep = "..")
        d <- stringi::stri_replace_all_regex(d, "\\.\\.NA", "")
      }
    }
    d
  })
}

collapse_sets <- function(x) {
  x <- lapply(x, paste, collapse = ",")
  x <- ifelse(stringi::stri_count_regex(x, ",") == 11 &
                stringi::stri_detect_regex(x, "-01-") &
                stringi::stri_detect_regex(x, "-12-"),
              stringi::stri_replace_all_regex(stringi::stri_extract_first_regex(x, "[^,]*"),
                                   "-01-", "-XX-"), x)
  x <- ifelse(stringi::stri_detect_regex(x, ","), paste0("{", x, "}"), x)
  x
}

collapse_ranges <- function(x) {
  x <- stringi::stri_replace_all_regex(x, "([:digit:]{4})-01-01\\.\\.([:digit:]{4})-12-31", "$1")
  x <- stringi::stri_replace_all_regex(x, "([:digit:]{4}-[:digit:]{2})-01\\.\\.([:digit:]{4}-[:digit:]{2})-28", "$1")
  x <- stringi::stri_replace_all_regex(x, "([:digit:]{4}-[:digit:]{2})-01\\.\\.([:digit:]{4}-[:digit:]{2})-29", "$1")
  x <- stringi::stri_replace_all_regex(x, "([:digit:]{4}-[:digit:]{2})-01\\.\\.([:digit:]{4}-[:digit:]{2})-30", "$1")
  x <- stringi::stri_replace_all_regex(x, "([:digit:]{4}-[:digit:]{2})-01\\.\\.([:digit:]{4}-[:digit:]{2})-31", "$1")
  x <- stringi::stri_replace_all_regex(x, "(-[:digit:]{4})-01-01\\.\\.(-[:digit:]{4})-12-31", "$1")
  x <- stringi::stri_replace_all_regex(x, "(-[:digit:]{3})-01-01\\.\\.(-[:digit:]{3})-12-31", "$1")
  x <- stringi::stri_replace_all_regex(x, "(-[:digit:]{4}-[:digit:]{2})-01\\.\\.(-[:digit:]{4}-[:digit:]{2})-28", "$1")
  x <- stringi::stri_replace_all_regex(x, "(-[:digit:]{4}-[:digit:]{2})-01\\.\\.(-[:digit:]{4}-[:digit:]{2})-29", "$1")
  x <- stringi::stri_replace_all_regex(x, "(-[:digit:]{4}-[:digit:]{2})-01\\.\\.(-[:digit:]{4}-[:digit:]{2})-30", "$1")
  x <- stringi::stri_replace_all_regex(x, "(-[:digit:]{4}-[:digit:]{2})-01\\.\\.(-[:digit:]{4}-[:digit:]{2})-31", "$1")
}

is_sequence <- function(x) {
  l <- as.Date(x) + 1
  l <- c(lubridate::NA_Date_, l[-length(l)])
  l <- x == l
  l[is.na(l)] <- FALSE
  l
}
