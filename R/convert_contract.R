# Contracts lists of dates into the shortest messy dates.
# This is the inverse of compatible_dates(). Users reach it through
# as_messydate() on a list. The deprecated contract() also calls it.
#
# With an mdate, the function first gives the possible dates of each element,
# and then contracts them. A '[]' set ("one member of") then stays a '[]' set.
# A list of dates always contracts to a '{}' set, as the list does not record
# the kind of set. A set whose members are consecutive days contracts to a
# range, '..', whichever notation it was given in.
# compatible_dates() drops the time of day from ranges, so a date-time range
# does not keep its times.
# With collapse = FALSE, a range that covers a whole month or year stays a
# range, "2012-06-01..2012-06-30", and does not become "2012-06".
.contract <- function(x, collapse = TRUE) {
  if (!inherits(x, 'list') && .has_missing(x))
    return(.with_missing(x, .contract, collapse = collapse))
  onesie <- rep(FALSE, length(x))
  if (!inherits(x, 'list')) {
    onesie <- stringi::stri_detect_regex(as.character(x), "^\\[.*\\]$")
    onesie[is.na(onesie)] <- FALSE
    x <- compatible_dates(x)
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

# Joins each run of consecutive days before the common era into a range.
# Days with gaps between them stay apart, so that a set stays a set.
compact_negative_dates <- function(x) {
  lapply(x, function(d) {
    if (length(d) < 2 || !isTRUE(stringi::stri_detect_regex(d[1], "^-")))
      return(d)
    num <- as.numeric(as.Date(new_messydate(d), FUN = identity))
    if (anyNA(num)) return(d)
    o <- order(num)
    d <- d[o]
    num <- num[o]
    run <- cumsum(c(1, diff(num) != 1))
    vapply(split(d, run), function(r)
      if (length(r) > 1) paste0(r[1], "..", r[length(r)]) else r,
      character(1), USE.NAMES = FALSE)
  })
}

compact_ranges <- function(x) {
  lapply(x, function(d) {
    # Days before the common era are already joined by
    # compact_negative_dates().
    if (length(d) > 1 && !any(stringi::stri_detect_regex(d, "^-|\\.\\."))) {
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
  # A range collapses only when both ends are in the same year (for a whole
  # year) or the same month (for a whole month): "\\1" is the year or month
  # of the first end.
  x <- stringi::stri_replace_all_regex(x, "(-?[:digit:]{3,4})-01-01\\.\\.\\1-12-31", "$1")
  .collapse_months(x)
}

# Collapses a range from the first to the last day of one month into that
# month. The last day must be the true last day of the month, so that
# "2012-02-01..2012-02-28" stays a range, as February 2012 has 29 days.
.collapse_months <- function(x) {
  rx <- "(-?[0-9]{4})-([0-9]{2})-01\\.\\.\\1-\\2-([0-9]{2})"
  hit <- !is.na(x) & stringi::stri_detect_regex(x, rx)
  x[hit] <- vapply(x[hit], function(s) {
    m <- stringi::stri_match_all_regex(s, rx)[[1]]
    for (k in seq_len(nrow(m))) {
      last <- days_in(as.integer(m[k, 2]), as.integer(m[k, 3]))
      if (as.integer(m[k, 4]) == last)
        s <- sub(m[k, 1], paste0(m[k, 2], "-", m[k, 3]), s, fixed = TRUE)
    }
    s
  }, character(1), USE.NAMES = FALSE)
  x
}

is_sequence <- function(x) {
  l <- as.Date(x) + 1
  l <- c(lubridate::NA_Date_, l[-length(l)])
  l <- x == l
  l[is.na(l)] <- FALSE
  l
}
