#' Coercion from common date classes to `mdate`
#' @description
#'   These methods coerce various date classes into the `mdate` class.
#'   They represent the main user-facing class-creating functions in the package.
#'   In addition to the typical date classes in R (`Date`, `POSIXct`, and `POSIXlt`),
#'   there is also a direct method for converting text or character strings to `mdate`.
#'   The function can also extract dates and times from text,
#'   including some historical prose conventions,
#'   though this is a work-in-progress and currently only works in English.
#' @param x A scalar or vector of a class that can be coerced into `mdate`,
#'   such as `Date`, `POSIXct`, `POSIXlt`, or character.
#' @param resequence Users have the option to choose the order for
#'   ambiguous dates with or without separators (e.g. "11-01-12" or "20112112").
#'   `NULL` by default.
#'   Other options include: 'dmy', 'ymd', 'mdy', 'ym', 'my' and 'interactive'
#'   If 'dmy', dates are converted from DDMMYY format for 6 digit dates,
#'   or DDMMYYYY format for 8 digit dates.
#'   If 'ymd', dates are converted from YYMMDD format for 6 digit dates,
#'   or YYYYMMDD format for 8 digit dates.
#'   If 'mdy', dates are converted from MMDDYY format for 6 digit dates
#'   or MMDDYYYY format for 8 digit dates.
#'   For these three options, ambiguous dates are converted to YY-MM-DD format
#'   for 6 digit dates, or YYYY-MM-DD format for 8 digit dates.
#'   If 'my', ambiguous 6 digit dates are converted from MM-YYYY format
#'   to YYYY-MM.
#'   If 'ym', ambiguous 6 digit dates are converted to YYYY-MM format.
#'   If 'interactive', it prompts users to select the existing
#'   component order of ambiguous dates,
#'   based on which the date is reordered into YYYY-MM-DD format
#'   and further completed to YYYY-MM-DD format if they choose to do so.
#' @return A `mdate` class object
#' @family coerce
#' @section Parsing prose:
#' Beyond plain and lightly-formatted dates, `as_messydate()` recognises
#' several conventions common in e.g. historical texts and converts them to
#' their ISO 8601-2 equivalent before the usual parsing takes place:
#' \itemize{
#'  \item{Roman numerals for a bare year, e.g. `"MDCCLXXVI"` becomes `1776`.}
#'  \item{Roman calendar references, i.e. the Kalends, Nones, and Ides of a
#'  named month, e.g. `"the Ides of March, 44 BC"` becomes `"-0043-03-15"`.
#'  The Nones and Ides fall later (the 7th and 15th) in March, May, July,
#'  and October, and earlier (the 5th and 13th) in other months.}
#'  \item{Approximate qualifiers ("around", "circa", "about", "roughly", ...)
#'  add the `~` annotation, and uncertain qualifiers ("possibly", "perhaps",
#'  "reportedly", ...) add `?`; both together add `%`, e.g.
#'  `"possibly about 1910"` becomes `"%1910"`.}
#'  \item{Connectives joining two days of the same month: "between the 13th
#'  and 15th" or "from the 13th to the 15th" become a range (`..`); "the
#'  13th or the 15th" becomes a set (`{}`); and a plain "the 13th and the
#'  15th" becomes two separate dates.}
#'  \item{"before"/"prior to"/"no later than" and "after"/"since"/"no
#'  earlier than" become an open range, e.g. `"before 1910"` becomes
#'  `"..1910"`. The bound may itself be any precision the parser
#'  understands, including a decade or century.}
#'  \item{Decades ("the 1920s" becomes `"192X"`) and centuries ("the 19th
#'  century" becomes `"18XX"`).}
#'  \item{A day of the year, e.g. `"the 103rd day of 2026"` becomes
#'  `"2026-04-13"`. The ISO 8601-2 notation for this (`"2026-103"`) is
#'  rejected instead of read, since it is easily read as a mistake for
#'  `"2026-10-03"`.}
#'  \item{A week of the year, e.g. `"the 5th week of 2026"` becomes the range
#'  `"2026-01-26..2026-02-01"`. Weeks follow the ISO 8601 rule: a week starts
#'  on a Monday, and week 1 is the week holding the first Thursday of the year,
#'  so week 1 can start in December of the previous year. The ISO 8601-2 week
#'  date is read too, as `"2026-W05"` or, for one day within the week,
#'  `"2026-W05-3"`, but it is never written back in that notation.}
#'  \item{Seasons, e.g. `"summer 2026"` becomes the month range
#'  `"2026-06..2026-08"`. These are the northern-hemisphere meteorological
#'  seasons, spring running from March to May and winter from December into
#'  February of the following year. The EDTF season codes (`"2026-22"`) are
#'  rejected instead of read, being easily read as a mistake for a date.}
#'  \item{A comma-separated list of dates in prose, e.g.
#'  `"13th Feb, 1977, Feb 15 1977, 1910"`, is split into separate dates
#'  (here, three): a fragment that is only a year is treated as completing
#'  the date before it.}
#' }
#' @section Eras and year numbering:
#' `{messydates}` stores years in the ISO 8601-2 (proleptic Gregorian)
#' astronomical numbering, in which a year zero exists and equals 1 BCE, `-0001`
#' equals 2 BCE, and so on. Historical "BC"/"BCE" prose uses the older
#' convention that has no year zero, so it is converted on input: a historical
#' year `N BCE` becomes the astronomical year `-(N-1)`, e.g. `"1 BCE"` becomes
#' `"0000"`, `"2 BCE"` becomes `"-0001"`, and `"44 BCE"` becomes `"-0043"`.
#' A year written directly in signed ISO form (e.g. `"-0044"`) is already
#' astronomical and is left unchanged, so `as_messydate("-0044")` (astronomical
#' year -44, i.e. 45 BCE) and `as_messydate("44 BCE")` (`"-0043"`) intentionally
#' differ. "AD"/"CE" prose is simply dropped, the year being unchanged.
#'
#' Era prose is a matter of input only: it is always removed on parsing, the
#' era of a date in an `mdate` being carried by the sign of its year alone.
#' Where it appears in the input, it is resolved for each year separately. A
#' marker written before a date governs that date (`"{BC1044-03-15,BC1033}"`);
#' otherwise a year takes the era of the first marker written after it, so a
#' marker given once at the end still applies to every bound of a range or set:
#' `"200..100 BC"`, `"..200 BC"` and `"200 BC.."` are all wholly BCE, whereas
#' `"200 BC..100 AD"` spans the two eras and gives `"-0199..0100"`.
#' @name coerce_to
NULL

#' @describeIn coerce_to Core `mdate` class coercion function
#' @examples
#' as_messydate("2021")
#' as_messydate("2021-02")
#' as_messydate("2021-02-01")
#' as_messydate("01-02-2021")
#' as_messydate("1 February 2021")
#' as_messydate("First of February, two thousand and twenty-one")
#' as_messydate("2021-02-01?")
#' as_messydate("2021-02-01~")
#' as_messydate("2021-02-01%")
#' as_messydate("2021-02-01..2021-02-28")
#' as_messydate("{2021-02-01,2021-02-28}")
#' as_messydate(c("-2021", "2021 BC", "-2021-02-01"))
#' as_messydate(c("210201", "20210201"), resequence = "ymd")
#' as_messydate(c("010221", "01022021"), resequence = "dmy")
#' # as_messydate(c("01-02-21", "01-02-2021", "01-02-91", "01-02-1991"),
#' # resequence = "interactive")
#' # ISO 8601-2 times, with the same annotations available on time components
#' as_messydate("2019-03-01 14:30:00Z")
#' as_messydate("2019-03-01 2:30pm")
#' as_messydate("2019-03-01 ~14:30")
#' # a time of day may also be given on its own, with no date part
#' as_messydate("2:30pm")
#' as_messydate("around 2pm")
#' # historical prose (see the "Parsing historical prose" section below)
#' as_messydate("MDCCLXXVI")
#' as_messydate("the Ides of March, 44 BC")
#' as_messydate("possibly about 1910")
#' as_messydate("the 1920s")
#' as_messydate("the 19th century")
#' as_messydate("the 103rd day of 2026")
#' as_messydate("the 5th week of 2026")
#' as_messydate("2026-W05")
#' as_messydate("summer 2026")
#' as_messydate("before 1910")
#' as_messydate("between the 13th and 15th of Feb, 1977")
#' @export
as_messydate <- function(x, resequence = FALSE)
  UseMethod("as_messydate")

#' @describeIn coerce_to Coerce from `Date` to `mdate` class
#' @export
as_messydate.Date <- function(x, resequence = FALSE) {
  # zero_padding() makes the year width canonical (four digits): R's
  # as.character.Date() does not always pad years below 1000 (and pads
  # negative, i.e. BCE, years to only three digits), which varies by platform.
  # Only non-NA elements are padded, both to leave NAs untouched and to avoid
  # ifelse() collapsing an all-NA vector to a logical (breaking is.character()).
  x <- as.character(x)
  ok <- !is.na(x)
  x[ok] <- zero_padding(x[ok])
  new_messydate(x)
}

#' @describeIn coerce_to Coerce from `POSIXct` to `mdate` class
#' @details
#'   Coercion from `POSIXct` and `POSIXlt` preserves the time of day
#'   (and UTC offset) as an ISO 8601-2 date-time.
#'   Times of exactly midnight (`00:00:00`) are treated as date-only,
#'   so that timezone-naive dates round-trip unchanged.
#' @export
as_messydate.POSIXct <- function(x, resequence = FALSE) {
  new_messydate(posix_to_iso(x))
}

#' @describeIn coerce_to Coerce from `POSIXlt` to `mdate` class
#' @export
as_messydate.POSIXlt <- function(x, resequence = FALSE) {
  new_messydate(posix_to_iso(as.POSIXct(x)))
}

# Formats a POSIXct vector as ISO 8601-2, keeping the time of day unless it
# is exactly midnight (in which case a date-only string is returned).
posix_to_iso <- function(x) {
  # Guarded explicitly, rather than relying on paste0()/ifelse() to handle
  # a zero-length x correctly: paste0() only returns character(0) when
  # *every* argument is zero-length, but .dt_sep is not, so
  # paste0(character(0), .dt_sep, character(0), character(0)) would
  # otherwise silently return .dt_sep (length 1) instead of character(0).
  if (length(x) == 0) return(character(0))
  # Built via direct assignment rather than ifelse(): ifelse() silently
  # returns the wrong type (logical, not character) when its test vector is
  # entirely NA, which as.POSIXct()/as.POSIXlt() values can legitimately be
  # (e.g. a zero-row `file.mtime()` result), and that then fails
  # new_messydate()'s is.character() check downstream.
  base <- format(x, "%Y-%m-%d")
  clock <- format(x, "%H:%M:%S")
  offset <- normalise_offset(format(x, "%z"))
  out <- paste0(base, .dt_sep, clock, offset)
  midnight <- !is.na(clock) & clock == "00:00:00"
  out[midnight] <- base[midnight]
  out[is.na(x)] <- NA_character_
  out[is.infinite(x)] <- "9999-12-31"
  out
}

# Normalises a numeric UTC offset ("+0000", "-0500", "") to ISO form
# ("Z", "-05:00", ""). UTC is written with the "Z" designator. Built with
# stringi (length-0/NA safe) and direct assignment rather than ifelse(),
# for the same reason as posix_to_iso() above.
normalise_offset <- function(z) {
  out <- stringi::stri_replace_first_regex(z, "^([+-][0-9]{2})([0-9]{2})$",
                                           "$1:$2")
  out[is.na(z) | z == ""] <- ""
  out[!is.na(z) & z == "+0000"] <- "Z"
  out
}

#' @export
as_messydate.mdate <- function(x, resequence = FALSE) {
  x <- as.character(x) # For updating 'mdate' variables
  new_messydate(x)
}

#' @describeIn coerce_to Coerce character date objects to `mdate` class
#' @export
as_messydate.character <- function(x, resequence = NULL) {
  if(any(is.infinite(x))) x[is.infinite(x)] <- "9999-12-31"
  # Refuse ISO 8601-2 forms this package cannot represent, rather than carrying
  # them along as strings nothing downstream can interpret.
  reject_unsupported(x)
  # Interpret historical prose cues (Roman calendar, qualifiers, connectives)
  # before the usual text extraction. This may lengthen the vector, e.g. when a
  # sentence lists several dates joined by "and".
  # The prose layer may return NA where it read the shape of a date but not a
  # possible one ("the 53rd week of 2026"), and may return more elements than
  # it was given, so the input is kept for reporting where it still lines up.
  orig <- x
  x <- interpret_prose(x)
  if (length(orig) != length(x)) orig <- x
  d <- standardise_text(x)
  # Protect any time-of-day substrings so the date pipeline (which repurposes
  # ':' as a range separator and '.' as a component separator) cannot mangle
  # them. Times are standardised separately and reattached at the end.
  prot <- protect_times(d)
  d <- prot$skeleton
  # Note which values were written as '[]' ("one member of") before the braces
  # are stripped, so they are not restored as '{}' ("all members of").
  onesie <- stringi::stri_detect_regex(d, "^\\s*\\[.*\\]\\s*$")
  onesie[is.na(onesie)] <- FALSE
  d <- standardise_date_separators(d)
  if (!is.null(resequence)) {
    if (resequence == "dmy") {
      d <- daymonthyear(d)
    } else if (resequence == "ymd") {
      d <- yearmonthday(d)
    } else if (resequence == "ym") {
      d <- yearmonth(d)
    } else if (resequence == "my") {
      d <- monthyear(d)
    } else if (resequence == "mdy") {
      d <- monthdayyear(d)
    } else if (isTRUE(resequence == "interactive")) {
      d <- ask_user(d)
    }
  }
  d <- standardise_date_order(d)
  d <- standardise_unspecifieds(d)
  d <- standardise_date_input(d)
  d <- standardise_widths(d)
  d <- restore_times(d, prot$times)
  if (any(onesie))
    d[onesie] <- stringi::stri_replace_all_regex(d[onesie], "^\\{(.*)\\}$", "[$1]")
  check_components(d, source = x)
  warn_unparsed(orig, d)
  new_messydate(d)
}

# Text that named no date at all comes back as NA. Say so, once, rather than
# letting an empty column look like missing data.
warn_unparsed <- function(x, d) {
  failed <- which(is.na(d) & !is.na(x) & nzchar(stringi::stri_trim_both(x)))
  if (length(failed) == 0) return(invisible(TRUE))
  warning(length(failed), " value", if (length(failed) > 1) "s" else "",
          " could not be parsed as a date and became NA:\n",
          report_elements(failed, x[failed]),
          "\nCall md_problems() to see all parsing problems.", call. = FALSE)
}

#' @describeIn coerce_to Coerce factors to `mdate` class, via their labels.
#'   This is the common case when a date column has been read in with
#'   `stringsAsFactors = TRUE`.
#' @export
as_messydate.factor <- function(x, resequence = NULL) {
  as_messydate(as.character(x), resequence)
}

#' @export
as_messydate.default <- function(x, resequence = NULL) {
  stop("Objects of class '", paste(class(x), collapse = "/"),
       "' cannot be coerced to 'mdate'. Convert to character, Date, or ",
       "POSIXct first.", call. = FALSE)
}

#' @describeIn coerce_to Coerce numeric objects to `mdate` class
#' @export
as_messydate.numeric <- function(x, resequence = NULL) {
  if(any(is.infinite(x))) x[is.infinite(x)] <- "9999-12-31"
  d <- as.character(x)
  new_messydate(d)
}

#' @describeIn coerce_to Coerce list date objects to the most concise
#' representation of `mdate` class
#' @examples
#' as_messydate(list(c("2012-06-01", "2012-06-02", "2012-06-03")))
#' as_messydate(list(c("2012-06-01", "2012-06-02", "2012-06-03",
#' "{2012-06-01, 2012-06-02, 2012-06-03}", "2012-06-01", "2012-06-03")))
#' @export
as_messydate.list <- function(x, resequence = FALSE) {
  lapply(x, function (y) {
    suppressMessages(contract(paste(new_messydate(as.character(y)),
                                    collapse = ",")))
  })
}

#' @rdname coerce_to
#' @export
mdate <- as_messydate

# Helper functions ####
#' @importFrom stringi stri_detect_regex
standardise_text <- function(v) {
  v <- convert_roman(v)
  # Drop ordinal suffixes on numeric days ("13th" -> "13") so both the text
  # extractor and the written-month parser see a plain number.
  v <- gsub("([0-9])(st|nd|rd|th)\\b", "\\1", v, ignore.case = TRUE, perl = TRUE)
  dates <- ifelse(stringi::stri_detect_regex(v, "([:alpha:]{4})") &
                    !grepl("bce$|^XXXX|XXXX$", v, ignore.case = TRUE),
                  extract_from_text(v), v)
  dates <- ifelse(grepl("Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec",
                        dates, ignore.case = TRUE),
                  written_month(dates), dates)
  dates
}

# Converts a bare Roman numeral (e.g. a year such as "MDCCLXXVI") to its
# integer form. Strings of only "X"s are left alone, since "X" marks an
# unspecified component.
convert_roman <- function(v) {
  isrom <- grepl("^[ivxlcdmIVXLCDM]+$", trimws(v)) & !grepl("^[Xx]+$", trimws(v))
  isrom[is.na(isrom)] <- FALSE
  if (any(isrom)) {
    num <- suppressWarnings(as.integer(utils::as.roman(trimws(v[isrom]))))
    v[isrom] <- ifelse(is.na(num), v[isrom], as.character(num))
  }
  v
}

#' @importFrom stringi stri_replace_all_regex
standardise_date_separators <- function(dates) {
  dates <- stringi::stri_replace_all_regex(dates,
                                    "(?<=[:digit:])\\.(?=[:digit:])", "-")
  dates <- stringi::stri_replace_all_regex(dates, "\\/", "-")
  dates <- stringi::stri_replace_all_regex(dates, "\\(|\\)|\\{|\\}|\\[|\\]", "")
  dates <- stringi::stri_trim_both(dates)
  # Adds zero padding to days, months, sets, and ranges
  dates <- stringi::stri_replace_all_regex(dates, "-([:digit:])-", "-0$1-")
  dates <- stringi::stri_replace_all_regex(dates, "([:digit:]{2})-([:digit:])$",
                                    "$1-0$2")
  dates <- stringi::stri_replace_all_regex(dates, "^([:digit:])-([:digit:]{2})",
                                    "0$1-$2")
  dates <- stringi::stri_replace_all_regex(dates, "\\_|\\:", "..") # range separators
  dates <- stringi::stri_replace_all_regex(dates, "-([:digit:])\\.\\.", "-0$1\\.\\.")
  dates <- stringi::stri_replace_all_regex(dates, "\\.\\.([:digit:])-", "\\.\\.0$1-")
  dates <- stringi::stri_replace_all_regex(dates, " \\, |\\, | \\,", ",") # set separators
  dates <- stringi::stri_replace_all_regex(dates, "-([:digit:]),", "-0$1,")
  dates <- stringi::stri_replace_all_regex(dates, ",([:digit:])-", "\\,0$1-")
  dates
}

daymonthyear <- function(dates) {
  dates <- ifelse(stringi::stri_detect_regex(dates, "^([:digit:]{2})-([:digit:]{2})-([:digit:]{2})$") &
                    as.numeric(gsub("-", "", stringi::stri_extract_first_regex(dates, "^[:digit:]{2}-"))) < 32 &
                    as.numeric(gsub("-", "", stringi::stri_extract_first_regex(dates, "-[:digit:]{2}-"))) < 32 &
                    as.numeric(gsub("-", "", stringi::stri_extract_first_regex(dates, "[:digit:]{2}$"))) < 32,
                  stringi::stri_replace_all_regex(dates, "^([:digit:]{2})-([:digit:]{2})-([:digit:]{2}$)",
                                           "$3-$2-$1"), dates)
  dates <- ifelse(stringi::stri_detect_regex(dates, "^([:digit:]{8})$"),
                  paste0(substr(dates, 5, 8), "-", substr(dates, 3, 4), "-",
                         substr(dates, 1, 2)), dates)
  dates <- ifelse(stringi::stri_detect_regex(dates, "^([:digit:]{6})$"),
                  paste0(substr(dates, 5, 6), "-", substr(dates, 3, 4), "-",
                         substr(dates, 1, 2)), dates)
  dates
}

yearmonthday <- function(dates) {
  dates <- ifelse(stringi::stri_detect_regex(dates, "^([:digit:]{8})$"),
                  paste0(substr(dates, 1, 4), "-", substr(dates, 5, 6), "-",
                         substr(dates, 7, 8)), dates)
  dates <- ifelse(stringi::stri_detect_regex(dates, "^([:digit:]{6})$"),
                  paste0(substr(dates, 1, 2), "-", substr(dates, 3, 4), "-",
                         substr(dates, 5, 6)), dates)
  dates
}

yearmonth <- function(dates) {
  dates <- ifelse(stringi::stri_detect_regex(dates, "^([:digit:]{6})$"),
                  paste0(substr(dates, 1, 4), "-", substr(dates, 5, 6)), dates)
  dates
}

monthyear <- function(dates) {
  dates <- ifelse(stringi::stri_detect_regex(dates, "^([:digit:]{6})$"),
                  paste0(substr(dates, 3, 6), "-", substr(dates, 1, 2)), dates)
  dates
}

monthdayyear <- function(dates) {
  dates <- ifelse(stringi::stri_detect_regex(dates, "^([:digit:]{2})-([:digit:]{2})-([:digit:]{2})$") &
                    as.numeric(gsub("-", "", stringi::stri_extract_first_regex(dates, "^[:digit:]{2}-"))) < 32 &
                    as.numeric(gsub("-", "", stringi::stri_extract_first_regex(dates, "-[:digit:]{2}-"))) < 32 &
                    as.numeric(gsub("-", "", stringi::stri_extract_first_regex(dates, "[:digit:]{2}$"))) < 32,
                  stringi::stri_replace_all_regex(dates, "^([:digit:]{2})-([:digit:]{2})-([:digit:]{2}$)",
                                           "$3-$1-$2"), dates)
  dates <- ifelse(stringi::stri_detect_regex(dates, "^([:digit:]{8})$"),
                  paste0(substr(dates, 5, 8), "-", substr(dates, 1, 2), "-",
                         substr(dates, 3, 4)), dates)
  dates <- ifelse(stringi::stri_detect_regex(dates, "^([:digit:]{6})$"),
                  paste0(substr(dates, 5, 6), "-", substr(dates, 1, 2), "-",
                         substr(dates, 3, 4)), dates)
  dates
}

standardise_date_order <- function(dates) {
  # if resequence argument is not specified, assumes ymd format
  dates <- ifelse(stringi::stri_detect_regex(dates, "^([:digit:]{8})$"),
                  paste0(substr(dates, 1, 4), "-", substr(dates, 5, 6), "-",
                         substr(dates, 7, 8)), dates)
  dates <- ifelse(stringi::stri_detect_regex(dates, "^([:digit:]{6})$"),
                  paste0(substr(dates, 1, 2), "-", substr(dates, 3, 4), "-",
                         substr(dates, 5, 6)), dates)
  # A month above 12 means the components are the other way round -- but only
  # if the day could itself be a month. "2019-13-45" is not a reordering
  # problem, and is left for check_components() to reject.
  dates <- ifelse(stringi::stri_detect_regex(dates, "^([:digit:]{4})-([:digit:]{2})-([:digit:]{2}$)") &
                    as.numeric(gsub("-", "", stringi::stri_extract_first_regex(dates, "-[:digit:]{2}-"))) > 12 &
                    as.numeric(stringi::stri_extract_last_regex(dates, "[:digit:]{2}$")) <= 12,
                  stringi::stri_replace_all_regex(dates, "^([:digit:]{4})-([:digit:]{2})-([:digit:]{2}$)",
                                           "$1-$3-$2"), dates)
  # detects and reorders inconsistencies
  dates <- ifelse(stringi::stri_detect_regex(dates, "^([:digit:]{2})-([:digit:]{2})-([:digit:]{4}$)") &
                    as.numeric(gsub("-", "", stringi::stri_extract_first_regex(dates, "-[:digit:]{2}-"))) > 12,
                  stringi::stri_replace_all_regex(dates, "^([:digit:]{2})-([:digit:]{2})-([:digit:]{4}$)",
                                           "$3-$1-$2"),
                  stringi::stri_replace_all_regex(dates, "^([:digit:]{2})-([:digit:]{2})-([:digit:]{4}$)",
                                           "$3-$2-$1"))
  dates <- ifelse(stringi::stri_detect_regex(dates, "^([:digit:]{2})-([:digit:]{2})-([:digit:]{2}$)") &
                    as.numeric(gsub("-", "", stringi::stri_extract_first_regex(dates, "-[:digit:]{2}-"))) < 13 &
                    as.numeric(gsub("-", "", stringi::stri_extract_first_regex(dates, "-[:digit:]{2}$"))) > 31,
                  stringi::stri_replace_all_regex(dates, "^([:digit:]{2})-([:digit:]{2})-([:digit:]{2}$)",
                                           "$3-$2-$1"), dates)
  dates <- ifelse(stringi::stri_detect_regex(dates, "^([:digit:]{2})-([:digit:]{2})-([:digit:]{2}$)") &
                    as.numeric(gsub("-", "", stringi::stri_extract_first_regex(dates, "-[:digit:]{2}-"))) > 12 &
                    as.numeric(gsub("-", "", stringi::stri_extract_first_regex(dates, "-[:digit:]{2}$"))) > 31,
                  stringi::stri_replace_all_regex(dates, "^([:digit:]{2})-([:digit:]{2})-([:digit:]{2}$)",
                                           "$3-$1-$2"), dates)
  dates
}

ask_user <- function(dates) {
  dates <- ifelse(stringi::stri_detect_regex(dates, "^([:digit:]{2})-([:digit:]{2})-([:digit:]{2})$") &
                    as.numeric(gsub("-", "", stringi::stri_extract_first_regex(dates, "^[:digit:]{2}-"))) < 32 &
                    as.numeric(gsub("-", "", stringi::stri_extract_first_regex(dates, "-[:digit:]{2}-"))) < 32 &
                    as.numeric(gsub("-", "", stringi::stri_extract_first_regex(dates, "[:digit:]{2}$"))) < 32,
                  reorder_ambiguous(dates), dates)
  dates <- ifelse(stringi::stri_detect_regex(dates, "^([:digit:]{2})-([:digit:]{2})-([:digit:]{2})$") &
                    as.numeric(gsub("-", "", stringi::stri_extract_first_regex(dates, "^[:digit:]{2}-"))) < 23,
                  complete_ambiguous_20(dates), dates)
  dates <- ifelse(stringi::stri_detect_regex(dates, "^([:digit:]{2})-([:digit:]{2})-([:digit:]{2})$") &
                    as.numeric(gsub("-", "", stringi::stri_extract_first_regex(dates, "^[:digit:]{2}-"))) > 22,
                  complete_ambiguous_19(dates), dates)
  dates
}

standardise_unspecifieds <- function(dates) {
  dates <- stringi::stri_replace_all_regex(dates, "^NA", "XXXX")
  dates <- stringi::stri_replace_all_regex(dates, "-NA", "-XX")
  # NB: a 4-digit year of "0000" denotes ISO 8601-2 astronomical year zero
  # (= 1 BCE), so it is *not* treated as unspecified; an unknown year uses
  # "XXXX" instead.
  dates <- stringi::stri_replace_all_regex(dates, "-00-|-0-|-0$|-00$|-\\?\\?-", "-XX-")
  dates <- stringi::stri_replace_all_regex(dates, "\\?\\?\\?\\?", "XXXX")
  dates <- stringi::stri_replace_all_regex(dates, "^(XX)-([:digit:]{4}$)", "$2")
  dates <- stringi::stri_replace_all_regex(dates, "^(XX)-(XX)-([:digit:]{4}$)", "$3")
  dates <- stringi::stri_replace_all_regex(dates, "^(XX)-([:digit:]{2})-([:digit:]{4}$)",
                                    "$3-$2")
  dates <- stringi::stri_replace_all_regex(dates, "^([:digit:]{2})-([:digit:]{2})-(XXXX$)",
                                    "$3-$2-$1")
  dates <- stringi::stri_replace_all_regex(dates, "-X-X$|-XX-XX$|-XX$|-XX-\\?\\?$|
                                    |-\\?-\\?$|-\\?\\?$|-\\?\\?-\\?\\?$", "")
  dates <- stringi::stri_replace_all_regex(dates, "-XX\\,", ",")
  dates <- stringi::stri_replace_all_regex(dates, "-XX\\.\\.", "..")
  dates <- ifelse(stringi::stri_detect_regex(dates, "^[:digit:]{4}\\~$"),
                  paste0("~", stringi::stri_replace_all_regex(dates, "\\~", "")), dates)
  dates
}

# Times ####

# The canonical date-time separator used in mdate output. ISO 8601-1 sec.
# 4.3.2 (and RFC 3339) both permit a space as an alternative to 'T'; a space
# is used here for readability. It cannot be confused with anything else in
# a canonical mdate string, since all other whitespace is stripped or
# squished away during parsing (see stri_squish()) -- so a bare space can
# only ever be this separator. 'T' continues to be accepted on input (see
# protect_times() below) and is tolerated, alongside a space, wherever a
# time is detected in an already-parsed mdate string, for robustness.
.dt_sep <- " "

# Sentinels that no date-pipeline regex touches (control characters, i.e. not
# digits, letters, or any of - . : , {} ~ ? % that the pipeline manipulates).
.time_open <- ""
.time_close <- ""

# A time-of-day token, introduced by 'T' (the ISO date-time separator).
# Accepts hh[:mm[:ss[.frac]]], optional am/pm, optional 'Z'/offset, and
# ISO 8601-2 uncertainty/approximation markers on or within the time.
.time_token_rx <- paste0(
  "T[~?%]?[0-9X]{1,2}",                    # hour (optional marker, digits/X)
  "(?::[~?%]?[0-9X]{1,2}){0,2}",           # optional :mm and :ss
  "(?:\\.[0-9]+)?",                        # optional fractional seconds
  "(?:[[:space:]]?[apAP][mM])?",           # optional am/pm
  "(?:Z|[+-][0-9]{1,2}(?::?[0-9]{2})?)?",  # optional Z or numeric offset
  "[~?%]?")                                # optional whole-time annotation

# Replaces each time substring in every element with an indexed sentinel and
# returns the standardised times separately, so the date pipeline runs on a
# skeleton that contains no time characters.
protect_times <- function(v) {
  skeleton <- v
  times <- vector("list", length(v))
  for (i in seq_along(v)) {
    s <- v[i]
    if (is.na(s)) {
      times[[i]] <- character(0)
      next
    }
    # Treat a space before a clock time ("2019-03-01 14:30") as the ISO 'T',
    # so the detection below (which requires a literal 'T') finds it. Either
    # the hour or the minute may carry an annotation (e.g.
    # "2019-03-01 ~14:30" or "2019-03-01 14:~30") or be unspecified
    # ("2019-03-01 XX:30"), matching .time_token_rx's own tolerance.
    s <- gsub("([0-9])[[:space:]]+([~?%]?[0-9X]{1,2}(?::[~?%]?[0-9X]|[[:space:]]?[apAP][mM]))",
              "\\1T\\2", s, perl = TRUE)
    # A time with no date part at all (the whole string is a clock time, e.g.
    # "2:30pm" or "14:30"). Prefix a 'T' so it is protected like any other
    # time; restore_times() reattaches it without a date-time separator.
    st <- trimws(s)
    if (!grepl("T", st) && grepl(.bare_time_rx, st, perl = TRUE)) s <- paste0("T", st)
    matches <- regmatches(s, gregexpr(.time_token_rx, s, perl = TRUE))[[1]]
    if (length(matches) == 0) {
      skeleton[i] <- s
      times[[i]] <- character(0)
      next
    }
    for (k in seq_along(matches)) {
      s <- sub(matches[k], paste0(.time_open, k, .time_close), s, fixed = TRUE)
    }
    times[[i]] <- vapply(matches, standardise_time, character(1),
                         USE.NAMES = FALSE)
    skeleton[i] <- s
  }
  list(skeleton = skeleton, times = times)
}

# Reinserts standardised times (with the canonical .dt_sep separator) in
# place of the sentinels left by protect_times().
restore_times <- function(v, times) {
  for (i in seq_along(v)) {
    if (length(times[[i]]) == 0) next
    for (k in seq_along(times[[i]])) {
      sentinel <- paste0(.time_open, k, .time_close)
      pos <- regexpr(sentinel, v[i], fixed = TRUE)
      # Only insert the date-time separator when a date component (digit or
      # 'X') immediately precedes the time. A bare time (sentinel at the very
      # start) is reattached on its own, with no leading separator.
      before <- if (pos > 1) substr(v[i], pos - 1, pos - 1) else ""
      sep <- if (grepl("[0-9X]", before)) .dt_sep else ""
      v[i] <- sub(sentinel, paste0(sep, times[[i]][k]), v[i], fixed = TRUE)
    }
  }
  v
}

# A bare time-of-day occupying a whole string, with no date part (e.g.
# "2:30pm", "14:30", "~14:30", "2pm+02:00"). Deliberately anchored to the
# whole (trimmed) string and requiring a strong time signal -- a colon-clock
# or an am/pm suffix -- so it can never misread the space before a second
# date in a set/list (e.g. "2012-01-01, 2012-02-02") as a time. A lone hour
# ("14") is not enough; without a date to anchor it, that is read as a year.
.bare_time_rx <- paste0(
  "^[~?%]?[0-9X]{1,2}",                          # hour (optional marker)
  "(?:",
  "(?::[~?%]?[0-9X]{1,2}){1,2}(?:\\.[0-9]+)?",   # :mm[:ss[.frac]] (colon clock)
  "(?:[[:space:]]?[apAP][mM])?",                 #   with optional am/pm
  "|[[:space:]]?[apAP][mM]",                     # or a bare am/pm hour
  ")",
  "(?:Z|[+-][0-9]{1,2}(?::?[0-9]{2})?)?",        # optional Z or numeric offset
  "[~?%]?$")                                     # optional whole-time marker

# A standardised (already parsed) time token, used to detect or strip times.
# Matches a leading space (the canonical separator) or 'T' (tolerated for
# robustness, e.g. a manually-constructed mdate string).
.std_time_rx <- paste0(
  "[T ][~?%]?[0-9X]{1,2}(:[~?%]?[0-9X]{1,2}){0,2}(\\.[0-9]+)?",
  "(Z|[+-][0-9]{2}:[0-9]{2})?[~?%]?")

# Removes time-of-day components from a canonical mdate string (per operand,
# so ranges and sets are handled), leaving the date part(s) intact.
strip_times <- function(x) {
  gsub(.std_time_rx, "", x)
}

# Standardises a single time token (leading 'T' included): zero-pads hour,
# minute, and second, converts am/pm to 24-hour, and normalises the offset.
standardise_time <- function(tok) {
  t <- sub("^T", "", tok)
  # Whole-time annotation (~ ? %) after any offset
  ann <- regmatches(t, regexpr("[~?%]$", t))
  t <- sub("[~?%]$", "", t)
  # UTC designator or numeric offset
  offset <- regmatches(t, regexpr("(Z|[+-][0-9]{1,2}(:?[0-9]{2})?)$", t))
  t <- sub("(Z|[+-][0-9]{1,2}(:?[0-9]{2})?)$", "", t)
  if (length(offset) && nzchar(offset) && offset != "Z") {
    sign <- substr(offset, 1, 1)
    digits <- gsub("[^0-9]", "", offset)
    oh <- substr(digits, 1, 2)
    om <- if (nchar(digits) > 2) substr(digits, 3, 4) else "00"
    if (nchar(oh) == 1) oh <- paste0("0", oh)
    offset <- if (oh == "00" && om == "00") "Z" else paste0(sign, oh, ":", om)
  }
  # am/pm
  ap <- tolower(regmatches(t, regexpr("[apAP][mM]$", t)))
  t <- sub("[[:space:]]?[apAP][mM]$", "", t)
  # Split into hour[:minute[:second]] (keeping any component annotations/X)
  parts <- strsplit(t, ":", fixed = TRUE)[[1]]
  parts <- vapply(parts, pad_time_component, character(1), USE.NAMES = FALSE)
  if (length(ap) && nzchar(ap)) {
    hr <- suppressWarnings(as.integer(gsub("[^0-9]", "", parts[1])))
    if (!is.na(hr)) {
      if (ap == "pm" && hr < 12) hr <- hr + 12
      if (ap == "am" && hr == 12) hr <- 0
      parts[1] <- sprintf("%02d", hr)
    }
    # A bare am/pm hour ("2pm") names an exact hour, so fill the minutes to
    # ":00" (unlike an ISO "T14", which stays at hour precision).
    if (length(parts) == 1L) parts <- c(parts, "00")
  }
  paste0(paste(parts, collapse = ":"), offset, ann)
}

# Zero-pads the numeric part of a single time component to two digits, leaving
# any leading annotation (~ ?), 'X' placeholders, or fractional part in place.
pad_time_component <- function(p) {
  pre <- regmatches(p, regexpr("^[~?%]*", p))
  rest <- sub("^[~?%]*", "", p)
  frac <- regmatches(rest, regexpr("\\.[0-9]+$", rest))
  num <- sub("\\.[0-9]+$", "", rest)
  if (grepl("^[0-9]$", num)) num <- paste0("0", num)
  paste0(pre, num, frac)
}

# BC/AD ####

.bc_rx <- "BCE|Bce|bce|bc|BC|Bc|bC"
.ce_rx <- "ad|AD|Ad|aD|CE|Ce|ce|AC|ac|Ac|aC"
# An era marker together with any whitespace in front of it, so removing it
# closes the gap it leaves ("200 BC..100 BC" -> "200..100"). 'BCE' must be
# offered before 'CE' for the leftmost-first alternation to read it as BC.
.era_rx <- paste0("[[:space:]]*(", .bc_rx, "|", .ce_rx, ")")
# The year of a date token: the first run of digits (with any leading
# annotation) that is not itself a continuation of a number. Excluding a
# preceding digit, '-' or time sentinel means month and day components
# ("-03-15"), already-astronomical years ("-199") and the index inside a
# protected time are all passed over.
.year_head_rx <- paste0("(?<![0-9\\-", .time_open, "])[~?%]*[0-9]+")

standardise_date_input <- function(dates) {
  dates <- vapply(dates, apply_era_markers, character(1), USE.NAMES = FALSE)
  dates <- stringi::stri_trim_both(dates)
  dates
}

# Applies (and removes) BC/BCE and AD/CE markers, deciding the era of each
# year in the expression separately. A marker written immediately before a
# date governs that date ("{BC2010-10-10,BC2010-10-11}"); otherwise a year
# takes the era of the first marker that follows it, so a marker written once
# at the end still governs every bound of a range or set ("200..100 BC" and
# "..200 BC" are wholly BCE), while "200 BC..100 AD" gives each bound its own
# era. Years with no marker after them, and strings with no marker at all,
# are read as CE and left unchanged.
apply_era_markers <- function(x) {
  if (is.na(x)) return(NA_character_)
  em <- gregexpr(.era_rx, x, perl = TRUE)[[1]]
  if (em[1] == -1L) return(x)
  estart <- as.integer(em)
  eend <- estart + attr(em, "match.length") # first character after the marker
  ebc <- grepl(paste0("^(", .bc_rx, ")$"),
               stringi::stri_trim_both(substring(x, estart, eend - 1L)))
  ym <- gregexpr(.year_head_rx, x, perl = TRUE)[[1]]
  if (ym[1] != -1L) {
    ystart <- as.integer(ym)
    yend <- ystart + attr(ym, "match.length")
    # Rewrite from the right, so the positions of the years still to be
    # considered (all of them to the left) stay valid.
    for (i in rev(seq_along(ystart))) {
      prev <- which(eend <= ystart[i])
      # A marker counts as attached to this year only if nothing but
      # whitespace separates them.
      attached <- length(prev) > 0L &&
        grepl("^[[:space:]]*$", substr(x, eend[max(prev)], ystart[i] - 1L))
      j <- if (attached) max(prev) else {
        nxt <- which(estart >= yend[i])
        if (length(nxt) > 0L) min(nxt) else NA_integer_
      }
      if (!is.na(j) && ebc[j])
        x <- paste0(substr(x, 1L, ystart[i] - 1L),
                    .hist_to_astro(substr(x, ystart[i], yend[i] - 1L)),
                    substring(x, yend[i]))
    }
  }
  stringi::stri_replace_all_regex(x, .era_rx, "")
}

# Converts BC-stripped, trimmed tokens (e.g. "44", "44-03-15", "1004-02",
# tolerating a leading ~/?/% and a trailing "..") from historical BCE reckoning
# to ISO 8601-2 astronomical year numbering, applying the shift to the leading
# year only and preserving month/day and range punctuation. Historical year Y
# (no year zero) maps to astronomical year V = 1 - Y, so 1 BCE -> 0 (later
# padded to "0000"), 2 BCE -> -1, 44 BCE -> -43. Vectorised; an element with no
# leading year is returned unchanged.
.hist_to_astro <- function(x) {
  pre <- stringi::stri_extract_first_regex(x, "^[~?%]*")
  pre[is.na(pre)] <- ""
  body <- stringi::stri_replace_first_regex(x, "^[~?%]*", "")
  yr <- suppressWarnings(
    as.integer(stringi::stri_extract_first_regex(body, "^[0-9]+")))
  rest <- stringi::stri_replace_first_regex(body, "^[0-9]+", "")
  v <- 1L - yr
  yout <- ifelse(v == 0L, "0", paste0("-", -v))
  ifelse(is.na(yr), x, paste0(pre, yout, rest))
}

# Widths ####

standardise_widths <- function(dates) {
  dates <- add_zero_padding(dates)
  dates <- ifelse(stringi::stri_detect_regex(dates, "([:digit:]{1})\\.\\.([:digit:]{1})|([:digit:]{1})\\.\\.-"),
                  add_zero_range(dates), dates)
  dates <- ifelse(stringi::stri_detect_regex(dates, "\\,"),
                  add_zero_set(dates), dates)
  dates <- ifelse(stringi::stri_detect_regex(dates, "\\,"),
                  paste0("{", dates, "}"), dates)
  dates <- stringi::stri_replace_all_regex(dates, "-([:digit:])$", "-0$1")
  dates <- stringi::stri_replace_all_regex(dates, "^([:digit:])-", "0$1-")
  dates <- stringi::stri_trim_both(dates)
  dates
}

add_zero_padding <- function(dates) {
  # Year padding (positive or negative)
  dates <- stringi::stri_replace_all_regex(dates, "^(-?)([:digit:]{1})($|-)", "$1000$2$3")
  dates <- stringi::stri_replace_all_regex(dates, "^(-?)([:digit:]{2})($|-)", "$100$2$3")
  dates <- stringi::stri_replace_all_regex(dates, "^(-?)([:digit:]{3})($|-)", "$10$2$3")
  # Uncertain and approximate year only
  dates <- stringi::stri_replace_all_regex(dates, "^~([:digit:]{1})$", "000$1~")
  dates <- stringi::stri_replace_all_regex(dates, "^~([:digit:]{2})$", "00$1~")
  dates <- stringi::stri_replace_all_regex(dates, "^~([:digit:]{3})$", "0$1~")
  dates <- stringi::stri_replace_all_regex(dates, "^\\?([:digit:]{1})$", "000$1?")
  dates <- stringi::stri_replace_all_regex(dates, "^\\?([:digit:]{2})$", "00$1?")
  dates <- stringi::stri_replace_all_regex(dates, "^\\?([:digit:]{3})$", "0$1?")
  dates <- ifelse(stringi::stri_detect_regex(dates,
                                      "^([:digit:]{1})~$|^([:digit:]{1})\\?$"),
                  paste0("000", dates), dates)
  dates <- ifelse(stringi::stri_detect_regex(dates,
                                      "^([:digit:]{2})~$|^([:digit:]{2})\\?$"),
                  paste0("00", dates), dates)
  dates <- ifelse(stringi::stri_detect_regex(dates,
                                      "^([:digit:]{3})~$|^([:digit:]{3})\\?$|^([:digit:]{3})-([:digit:]{2}$)"),
                  paste0("0", dates), dates)
  # Year only
  dates <- stringi::stri_replace_all_regex(dates, "^([:digit:]{1})$", "000$1")
  dates <- stringi::stri_replace_all_regex(dates, "^([:digit:]{2})$", "00$1")
  dates <- stringi::stri_replace_all_regex(dates, "^([:digit:]{3})$", "0$1")
  dates
}

add_zero_range <- function(dates) {
  dates <- strsplit(dates, "\\.\\.")
  dates <- lapply(dates, function(x) {
    x <- gsub(" ", "", x)
    x <- stringi::stri_replace_all_regex(x, "^-([:digit:]{1})$", "-000$1")
    x <- stringi::stri_replace_all_regex(x, "^-([:digit:]{2})$", "-00$1")
    x <- stringi::stri_replace_all_regex(x, "^-([:digit:]{3})$", "-0$1")
    x <- stringi::stri_replace_all_regex(x, "^~([:digit:]{1})$", "000$1~")
    x <- stringi::stri_replace_all_regex(x, "^~([:digit:]{2})$", "00$1~")
    x <- stringi::stri_replace_all_regex(x, "^~([:digit:]{3})$", "0$1~")
    x <- stringi::stri_replace_all_regex(x, "^\\?([:digit:]{1})$", "000$1?")
    x <- stringi::stri_replace_all_regex(x, "^\\?([:digit:]{2})$", "00$1?")
    x <- stringi::stri_replace_all_regex(x, "^\\?([:digit:]{3})$", "0$1?")
    x <- stringi::stri_replace_all_regex(x, "^([:digit:]{1})$", "000$1")
    x <- stringi::stri_replace_all_regex(x, "^([:digit:]{2})$", "00$1")
    x <- stringi::stri_replace_all_regex(x, "^([:digit:]{3})$", "0$1")
    x <- ifelse(stringi::stri_detect_regex(x, "^([:digit:]{1})~$|^([:digit:]{1})\\?$"),
                paste0("000", x), x)
    x <- ifelse(stringi::stri_detect_regex(x, "^([:digit:]{2})~$|^([:digit:]{2})\\?$"),
                paste0("00", x), x)
    x <- ifelse(stringi::stri_detect_regex(x, "^([:digit:]{3})~$|^([:digit:]{3})\\?$"),
                paste0("0", x), x)
    x <- ifelse(stringi::stri_detect_regex(x, "^([:digit:]{1})~$|^([:digit:]{1})\\?$"),
                paste0("000", x), x)
    x <- ifelse(stringi::stri_detect_regex(x, "^([:digit:]{2})~$|^([:digit:]{2})\\?$"),
                paste0("00", x), x)
    x <- ifelse(stringi::stri_detect_regex(x, "^([:digit:]{3})~$|^([:digit:]{3})\\?$|^([:digit:]{3})-([:digit:]{2}$)"),
                paste0("0", x), x)
  })
  dates <- vapply(dates, paste, character(1), collapse = "..", USE.NAMES = FALSE)
  dates
}

add_zero_set <- function(dates) {
  dates <- strsplit(dates, "\\,")
  dates <- lapply(dates, function(x) {
    x <- gsub(" ", "", x)
    x <- stringi::stri_replace_all_regex(x, "^-([:digit:]{1})$", "-000$1")
    x <- stringi::stri_replace_all_regex(x, "^-([:digit:]{2})$", "-00$1")
    x <- stringi::stri_replace_all_regex(x, "^-([:digit:]{3})$", "-0$1")
    x <- stringi::stri_replace_all_regex(x, "^~([:digit:]{1})$", "000$1~")
    x <- stringi::stri_replace_all_regex(x, "^~([:digit:]{2})$", "00$1~")
    x <- stringi::stri_replace_all_regex(x, "^~([:digit:]{3})$", "0$1~")
    x <- stringi::stri_replace_all_regex(x, "^\\?([:digit:]{1})$", "000$1?")
    x <- stringi::stri_replace_all_regex(x, "^\\?([:digit:]{2})$", "00$1?")
    x <- stringi::stri_replace_all_regex(x, "^\\?([:digit:]{3})$", "0$1?")
    x <- stringi::stri_replace_all_regex(x, "^([:digit:]{1})$", "000$1")
    x <- stringi::stri_replace_all_regex(x, "^([:digit:]{2})$", "00$1")
    x <- stringi::stri_replace_all_regex(x, "^([:digit:]{3})$", "0$1")
    x <- ifelse(stringi::stri_detect_regex(x, "^([:digit:]{1})~$|^([:digit:]{1})\\?$"),
                paste0("000", x), x)
    x <- ifelse(stringi::stri_detect_regex(x, "^([:digit:]{2})~$|^([:digit:]{2})\\?$"),
                paste0("00", x), x)
    x <- ifelse(stringi::stri_detect_regex(x, "^([:digit:]{3})~$|^([:digit:]{3})\\?$|^([:digit:]{3})-([:digit:]{2}$)"),
                paste0("0", x), x)
  })
  dates <- vapply(dates, paste, character(1), collapse = ",", USE.NAMES = FALSE)
  dates
}

# Natural-language interpretation of historical date prose. Recognises Roman
# calendar references (Kalends/Nones/Ides), approximate or uncertain qualifiers
# ("around", "circa", "possibly"), and connectives implying a range
# ("between .. and .."), a set (".. or .."), or several dates (".. and ..").
# Strings without such cues are returned unchanged for the normal text parser.
interpret_prose <- function(x) {
  unlist(lapply(x, interpret_one), use.names = FALSE)
}

.nl_months <- c(january = 1, february = 2, march = 3, april = 4, may = 5,
                june = 6, july = 7, august = 8, september = 9, october = 10,
                november = 11, december = 12, jan = 1, feb = 2, mar = 3,
                apr = 4, jun = 6, jul = 7, aug = 8, sept = 9, sep = 9,
                oct = 10, nov = 11, dec = 12)

nl_month_num <- function(s) {
  m <- stringi::stri_extract_first_regex(
    tolower(s), paste(names(.nl_months), collapse = "|"))
  if (is.na(m)) NA_integer_ else unname(.nl_months[m])
}

# The last (right-most) number in the string, taken to be the year.
nl_year <- function(s) {
  y <- stringi::stri_extract_last_regex(s, "[0-9]{3,4}")
  if (is.na(y)) y <- stringi::stri_extract_last_regex(s, "[0-9]{1,4}")
  y
}

# Day-of-month for a Roman calendar reference. Nones and Ides fall later in
# March, May, July, and October.
roman_calendar_day <- function(kind, mn) {
  long <- mn %in% c(3, 5, 7, 10)
  switch(kind,
         kalends = 1L,
         nones = if (long) 7L else 5L,
         ides = if (long) 15L else 13L)
}

# Resolves the date phrase that follows "before"/"after" by reusing the full
# parser, so open ranges can carry any precision (year, month, or full date).
inner_date <- function(s) {
  s <- trimws(s)
  if (!nzchar(s)) return(s)
  as.character(suppressWarnings(as_messydate(s)))
}

# One end of a bound that is itself a range, so that an open range keeps a
# single bound: "before the 5th week of 2026" is "..2026-01-26", not
# "..2026-01-26..2026-02-01".
range_bound <- function(d, bound) {
  if (is.na(d) || !grepl("..", d, fixed = TRUE)) return(d)
  parts <- strsplit(d, "..", fixed = TRUE)[[1]]
  parts <- parts[nzchar(parts)]
  if (length(parts) == 0) return(d)
  if (bound == "min") parts[1] else parts[length(parts)]
}

# Regroups comma-separated fragments into whole dates: a fragment that is only
# a year is attached to the preceding fragment when that one lacks a year, so
# "13th Feb", "1977", "Feb 15 1977", "1910" becomes three dates.
rejoin_date_fragments <- function(frags) {
  frags <- trimws(frags)
  frags <- frags[nzchar(frags)]
  out <- character(0)
  cur <- ""
  for (f in frags) {
    if (!nzchar(cur)) {
      cur <- f
    } else if (grepl("^[0-9]{3,4}$", f) && !grepl("[0-9]{4}", cur)) {
      cur <- paste(cur, f)
    } else {
      out <- c(out, cur)
      cur <- f
    }
  }
  if (nzchar(cur)) out <- c(out, cur)
  out
}

# Month range for a (northern-hemisphere meteorological) season in a given
# year. Winter spans into the following February.
season_range <- function(season, yr) {
  switch(season,
         spring = sprintf("%s-03..%s-05", yr, yr),
         summer = sprintf("%s-06..%s-08", yr, yr),
         autumn = ,
         fall   = sprintf("%s-09..%s-11", yr, yr),
         winter = sprintf("%s-12..%04d-02", yr, as.integer(yr) + 1L))
}

# Number of days in a year, following the leap rule used by days_in().
days_in_year <- function(y) {
  365L + as.integer(y %% 4 == 0 & (y %% 100 != 0 | y %% 400 == 0))
}

# Calendar date of the nth day of a year, e.g. (2026, 103) gives "2026-04-13".
# A number outside the year rolls into the neighbouring one, so that the week
# helpers below can share this. Built on days_in() rather than as.Date(), so
# that it holds for any year the package accepts, including a negative one.
ordinal_to_date <- function(y, n) {
  while (n < 1) {
    y <- y - 1L
    n <- n + days_in_year(y)
  }
  while (n > days_in_year(y)) {
    n <- n - days_in_year(y)
    y <- y + 1L
  }
  cum <- cumsum(days_in(y, 1:12))
  mn <- which(n <= cum)[1]
  dy <- n - c(0, cum)[mn]
  sprintf("%s%04d-%02d-%02d", if (y < 0) "-" else "", abs(y), mn, dy)
}

# Day number of a proleptic Gregorian date, counted from 1970-01-01. This is
# the era algorithm, which stays correct for negative years, where as.Date()
# and so lubridate::isoweek() do not reach.
days_from_civil <- function(y, m, d) {
  y <- y - (m <= 2)
  era <- y %/% 400 # R's %/% floors, which is what the algorithm needs here
  yoe <- y - era * 400
  doy <- (153 * (m + ifelse(m > 2, -3, 9)) + 2) %/% 5 + d - 1
  doe <- yoe * 365 + yoe %/% 4 - yoe %/% 100 + doy
  era * 146097 + doe - 719468
}

# Day of the week, 1 for Monday to 7 for Sunday. 1970-01-01 was a Thursday.
iso_weekday <- function(y, m, d) ((days_from_civil(y, m, d) + 3) %% 7) + 1

# An ISO year has 53 weeks when it starts on a Thursday, or when it is a leap
# year starting on a Wednesday. Otherwise it has 52.
weeks_in_isoyear <- function(y) {
  jan1 <- iso_weekday(y, 1, 1)
  if (jan1 == 4 || (jan1 == 3 && days_in_year(y) == 366)) 53L else 52L
}

# Day number of the Monday that opens ISO week w of year y, or NA where the
# year has no such week. The 4th of January always falls in week 1.
iso_week_start <- function(y, w) {
  if (is.na(y) || is.na(w) || w < 1 || w > weeks_in_isoyear(y))
    return(NA_integer_)
  days_from_civil(y, 1, 4) - (iso_weekday(y, 1, 4) - 1) + 7 * (w - 1)
}

# An ISO week as the range of its seven days. Week 1 can start in December of
# the previous year, which ordinal_to_date() resolves.
iso_week_range <- function(y, w) {
  z <- iso_week_start(y, w)
  if (is.na(z)) return(NA_character_)
  jan1 <- days_from_civil(y, 1, 1)
  paste0(ordinal_to_date(y, z - jan1 + 1), "..",
         ordinal_to_date(y, z - jan1 + 7))
}

# One weekday within an ISO week, wd running from 1 (Monday) to 7 (Sunday).
iso_week_date <- function(y, w, wd) {
  z <- iso_week_start(y, w)
  if (is.na(z)) return(NA_character_)
  ordinal_to_date(y, z - days_from_civil(y, 1, 1) + wd)
}

# The number and the year in "the 5th week of 2026" or "week 5 of 2026",
# returned as c(year, number), or NULL where the phrase has neither shape.
# The word "of" or "in" is required, so that a duration such as "5 weeks" or
# "3 days later" is left to the rest of the parser.
year_part_number <- function(low, unit) {
  m <- stringi::stri_match_first_regex(
    low, paste0("\\b([0-9]{1,3})(?:st|nd|rd|th)?\\s+", unit,
                "s?\\b\\s+(?:of|in)\\s+(?:the\\s+year\\s+)?([0-9]{3,4})\\b"))
  if (is.na(m[1, 1]))
    m <- stringi::stri_match_first_regex(
      low, paste0("\\b", unit, "\\s+([0-9]{1,3})\\s+(?:of|in)\\s+",
                  "(?:the\\s+year\\s+)?([0-9]{3,4})\\b"))
  if (is.na(m[1, 1])) return(NULL)
  c(as.integer(m[1, 3]), as.integer(m[1, 2]))
}

# Carries any approximate or uncertain qualifier in the phrase onto each bound
# of an already resolved date or range ("around the 5th week of 2026").
qualify_bounds <- function(d, low) {
  if (is.na(d)) return(NA_character_)
  approx <- grepl(paste0("\\b(", .approx_words_rx, ")\\b"), low)
  uncert <- grepl(paste0("\\b(", .uncert_words_rx, ")\\b"), low)
  qual <- if (approx && uncert) "%" else if (approx) "~" else
    if (uncert) "?" else ""
  if (!nzchar(qual)) return(d)
  paste0(strsplit(d, "..", fixed = TRUE)[[1]], qual, collapse = "..")
}

# Prose qualifier words (approximate and/or uncertain), shared between the
# qualifier-detection branch of interpret_one() and its leading-"at" stripper.
.approx_words_rx <- "around|circa|approx|approximately|about|roughly|estimated"
.uncert_words_rx <- "possibly|perhaps|maybe|uncertain|reportedly|allegedly"

interpret_one <- function(s) {
  if (is.na(s) || !is.character(s)) return(s)
  low <- tolower(s)

  # "at 2:30pm" / "at around 2pm": "at" is just a preposition introducing a
  # time, but it defeats the anchored bare-time match (.bare_time_rx) below.
  # Strip it whenever what remains -- once any qualifier word is set aside --
  # is itself a bare time, so both "at 2:30pm" and "at around 2pm" reach the
  # normal bare-time handling.
  if (grepl("^at\\s+", low)) {
    rest <- sub("^at\\s+", "", low)
    plain <- trimws(gsub(paste0("\\b(", .approx_words_rx, "|", .uncert_words_rx, ")\\b"),
                         " ", rest, perl = TRUE))
    if (grepl(.bare_time_rx, plain, perl = TRUE)) {
      s <- sub("(?i)^at\\s+", "", s, perl = TRUE)
      low <- rest
    }
  }

  # Open ranges (checked first, so the bound may itself be a decade/century):
  # "before 1910" -> "..1910"; "after the 1920s" -> "192X..".
  if (grepl("\\b(before|prior to|no later than)\\b", low)) {
    d <- inner_date(sub(".*\\b(?:before|prior to|no later than)\\b", "", low,
                        perl = TRUE))
    return(paste0("..", range_bound(d, "min")))
  }
  if (grepl("\\b(after|since|no earlier than)\\b", low)) {
    d <- inner_date(sub(".*\\b(?:after|since|no earlier than)\\b", "", low,
                        perl = TRUE))
    return(paste0(range_bound(d, "max"), ".."))
  }

  # Century, e.g. "19th century" -> "18XX" (the 19th century is 1800-1899).
  cen <- stringi::stri_match_first_regex(low, "([0-9]+)(?:st|nd|rd|th)?\\s+centur")
  if (!is.na(cen[1, 1]))
    return(sprintf("%02dXX", as.integer(cen[1, 2]) - 1L))

  # Decade, e.g. "1910s" -> "191X".
  dec <- stringi::stri_match_first_regex(low, "\\b([0-9]{3})0s\\b")
  if (!is.na(dec[1, 1])) return(paste0(dec[1, 2], "X"))

  # A day of the year, e.g. "the 103rd day of 2026" gives "2026-04-13", and an
  # ISO week, e.g. "the 5th week of 2026" gives "2026-01-26..2026-02-01". The
  # ISO 8601-2 week date "2026-W05", with or without a weekday ("2026-W05-3"),
  # is read the same way, but is never written back in that notation. Only a
  # plain 3- or 4-digit year is read from prose, as for the seasons below, so
  # era prose such as "the 103rd day of 44 BC" is not covered.
  wk <- stringi::stri_match_first_regex(
    s, "^\\s*(-?[0-9]{4})-[Ww]([0-9]{1,2})(?:-([1-7]))?\\s*$")
  if (!is.na(wk[1, 1])) {
    y <- as.integer(wk[1, 2])
    w <- as.integer(wk[1, 3])
    if (is.na(wk[1, 4])) return(iso_week_range(y, w))
    return(iso_week_date(y, w, as.integer(wk[1, 4])))
  }
  prt <- year_part_number(low, "week")
  if (!is.null(prt)) return(qualify_bounds(iso_week_range(prt[1], prt[2]), low))
  prt <- year_part_number(low, "day")
  if (!is.null(prt)) {
    ok <- prt[2] >= 1 && prt[2] <= days_in_year(prt[1])
    d <- if (ok) ordinal_to_date(prt[1], prt[2]) else NA_character_
    return(qualify_bounds(d, low))
  }

  # Seasons and relative parts of a year, expressed as month ranges rather than
  # EDTF season codes (deliberately undocumented; only applied to a plain year,
  # so a decade or century keeps precedence above). Northern-hemisphere
  # meteorological seasons; thirds of the year for early/mid/late.
  seas <- stringi::stri_match_first_regex(
    low, "\\b(spring|summer|autumn|fall|winter)\\b\\s+(?:of\\s+)?([0-9]{3,4})")
  if (!is.na(seas[1, 1])) return(season_range(seas[1, 2], seas[1, 3]))
  emr <- stringi::stri_match_first_regex(
    low, "\\b(early|mid|late)\\b\\s+(?:in\\s+|the\\s+)?([0-9]{3,4})")
  if (!is.na(emr[1, 1])) {
    r <- switch(emr[1, 2], early = c("01", "04"), mid = c("05", "08"),
                late = c("09", "12"))
    return(sprintf("%s-%s..%s-%s", emr[1, 3], r[1], emr[1, 3], r[2]))
  }

  mn <- nl_month_num(s)
  has_roman_cal <- grepl("\\b(kalends|nones|ides)\\b", low)

  # 0. A prose list of several dates separated by commas, e.g.
  # "13th Feb, 1977, Feb 15 1977, 1910". Only triggered for prose (a month name
  # present) with two or more four-digit years, so ISO sets/ranges are left to
  # the normal parser. Each date is parsed on its own.
  if (grepl(",", s) && !is.na(mn) && !grepl("[{}\\[\\]]|\\.\\.", s) &&
      lengths(regmatches(s, gregexpr("[0-9]{4}", s))) >= 2) {
    frags <- rejoin_date_fragments(strsplit(s, ",")[[1]])
    if (length(frags) >= 2)
      return(unlist(lapply(frags,
                           function(f) as.character(as_messydate(trimws(f))))))
  }

  # 1. Roman calendar reference, e.g. "the Ides of March, 44 BC".
  if (has_roman_cal && !is.na(mn)) {
    kind <- stringi::stri_extract_first_regex(low, "kalends|nones|ides")
    yr <- nl_year(s)
    bc <- grepl("\\bbce?\\b|\\bbc\\b", low)
    return(sprintf("%04d-%02d-%02d%s", as.integer(yr), mn,
                   roman_calendar_day(kind, mn), if (bc) " BC" else ""))
  }

  # 2. Two days joined by a connective ("between the 13th and 15th of Feb").
  dl <- stringi::stri_match_first_regex(
    low, "([0-9]{1,2})(?:st|nd|rd|th)?\\s+(to|and|or)\\s+(?:the\\s+)?([0-9]{1,2})(?:st|nd|rd|th)?")
  if (!is.na(dl[1, 1]) && !is.na(mn)) {
    conn <- dl[1, 3]
    between <- grepl("\\bbetween\\b", low)
    yr <- nl_year(s)
    d1 <- sprintf("%s-%02d-%02d", yr, mn, as.integer(dl[1, 2]))
    d2 <- sprintf("%s-%02d-%02d", yr, mn, as.integer(dl[1, 4]))
    if (between || conn == "to") return(paste0(d1, "..", d2))
    if (conn == "or") return(paste0("{", d1, ",", d2, "}"))
    return(c(d1, d2)) # a plain "and" lists several separate dates
  }

  # 3. A single date carrying an approximate and/or uncertain qualifier
  # (both together give the EDTF "%" marker).
  approx_words <- .approx_words_rx
  uncert_words <- .uncert_words_rx
  approx <- grepl(paste0("\\b(", approx_words, ")\\b"), low)
  uncert <- grepl(paste0("\\b(", uncert_words, ")\\b"), low)
  qual <- if (approx && uncert) "%" else if (approx) "~" else if (uncert) "?" else NA_character_
  if (!is.na(qual)) {
    # Strip the qualifier words before reading the date, so a qualifier that
    # embeds a month name (the "may" inside "maybe") cannot be mistaken for a
    # month, and the residual is a clean date phrase.
    bare <- trimws(gsub(paste0("\\b(", approx_words, "|", uncert_words, ")\\b"),
                        " ", low, perl = TRUE))
    # A qualified bare time ("around 2pm"): carry the qualifier as a whole-time
    # annotation and let the time pipeline standardise it (-> "14:00~").
    if (grepl(.bare_time_rx, bare, perl = TRUE)) return(paste0(bare, qual))
    # The returns below rebuild the date from its components, so any era
    # marker has to be carried over explicitly; standardise_date_input()
    # applies it later ("circa 200 BC" -> "~200 BC" -> "~-199").
    era <- stringi::stri_extract_first_regex(s, paste0("(", .bc_rx, "|", .ce_rx, ")"))
    era <- if (is.na(era)) "" else paste0(" ", era)
    yr <- nl_year(bare)
    mnb <- nl_month_num(bare)
    # An explicit ordinal day keeps the qualifier on that day component
    # (e.g. "around the 13th of Feb 1977" -> "1977-02-~13").
    day <- stringi::stri_extract_first_regex(bare, "[0-9]{1,2}(?=st|nd|rd|th)")
    if (!is.na(mnb) && !is.na(day) && !identical(day, yr))
      return(paste0(sprintf("%s-%02d-%s%02d", yr, mnb, qual, as.integer(day)),
                    era))
    # A fully specified date (numeric ISO or written month) keeps its month and
    # day; the qualifier applies to the whole date, as a suffix
    # (e.g. "approximately 2024-01-22" -> "2024-01-22~").
    iso <- inner_date(bare)
    if (!is.na(iso) && iso != "NA" && grepl("-", sub("^-", "", iso)))
      return(paste0(iso, qual, era))
    # Otherwise only a year is known: prefix it (e.g. "circa 2012" -> "~2012").
    if (!is.na(yr)) return(paste0(qual, yr, era))
  }

  s
}

extract_from_text <- function(v) {
  # Drop ordinal suffixes on numeric days ("4th" -> "4", "22nd" -> "22").
  v <- gsub("([0-9])(st|nd|rd|th)\\b", "\\1", v, ignore.case = TRUE, perl = TRUE)
  # "last day of <month>" -> the last day number of that month.
  v <- replace_last_day(v)
  # get ordinal and numeric dates spelled and replace in text
  out <- stri_squish(stringi::stri_replace_all_regex(v, "\\,|\\.|of | on | and|the | this|
                                  | day|year|month", " "))
  # Reorder month-first American dates ("July 4 1976" -> "4 July 1976"); a
  # day-first phrase ("Fourth of July") already leads with the day.
  first_tok <- stringi::stri_extract_first_regex(out, "^\\S+")
  reorder <- !is.na(first_tok) &
    grepl("^(jan|feb|mar|apr|may|jun|jul|aug|sep|oct|nov|dec)", first_tok,
          ignore.case = TRUE) &
    stringi::stri_count_regex(out, "\\S+") >= 3
  reorder[is.na(reorder)] <- FALSE
  if (any(reorder)) {
    out[reorder] <- vapply(stringi::stri_split_fixed(out[reorder], " "),
                           function(tok) paste(tok[c(2, 1, 3)], collapse = " "),
                           character(1))
  }

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
  out <- stri_squish(stringi::stri_replace_all_regex(out, "- -| -|- |/", "-"))
  # get the first date per row
  pre <- out
  out <- stringi::stri_extract_first_regex(pre,
                              "^[:digit:]{2}-[:digit:]{2}-[:digit:]{2}$|
                              |^[:digit:]{1}-[:digit:]{2}-[:digit:]{2}$|
                              |^[:digit:]{2}-[:digit:]{1}-[:digit:]{2}$|
                              |^[:digit:]{1}-[:digit:]{1}-[:digit:]{2}$|
                              |[:digit:]{4}-[:digit:]{2}-[:digit:]{2}|
                              |[:digit:]{4}-[:digit:]{2}-[:digit:]{1}|
                              |[:digit:]{4}-[:digit:]{1}-[:digit:]{2}|
                              |[:digit:]{4}-[:digit:]{1}-[:digit:]{1}|
                              |[:digit:]{2}-[:digit:]{2}-[:digit:]{4}|
                              |[:digit:]{1}-[:digit:]{2}-[:digit:]{4}|
                              |[:digit:]{2}-[:digit:]{1}-[:digit:]{4}|
                              |[:digit:]{1}-[:digit:]{1}-[:digit:]{4}")
  # Fallback: a converted month (dash-flanked) and a year, but no day, give a
  # year-month value (e.g. "February 2004" -> "2004-02").
  need <- which(is.na(out) & !is.na(pre))
  if (length(need)) {
    mo <- stringi::stri_match_first_regex(pre[need], "-([:digit:]{2})-")[, 2]
    yr <- stringi::stri_extract_first_regex(pre[need], "[:digit:]{4}")
    ok <- !is.na(mo) & !is.na(yr)
    out[need[ok]] <- paste0(yr[ok], "-", mo[ok])
  }
  out
}

# Replaces the word "last" with the last day number of the month named in the
# same string (e.g. "Last day of July" -> "31 day of July"). For February the
# year, if present, decides between 28 and 29.
replace_last_day <- function(v) {
  mon_last <- c(jan = 31, feb = 28, mar = 31, apr = 30, may = 31, jun = 30,
                jul = 31, aug = 31, sep = 30, oct = 31, nov = 30, dec = 31)
  vapply(v, function(s) {
    if (is.na(s) || !grepl("\\blast\\b", s, ignore.case = TRUE, perl = TRUE))
      return(s)
    m3 <- tolower(substr(stringi::stri_extract_first_regex(
      s, "(?i)jan|feb|mar|apr|may|jun|jul|aug|sep|oct|nov|dec"), 1, 3))
    if (is.na(m3) || !m3 %in% names(mon_last)) return(s)
    last <- mon_last[[m3]]
    if (m3 == "feb") {
      yr <- suppressWarnings(as.integer(
        stringi::stri_extract_first_regex(s, "[0-9]{4}")))
      if (!is.na(yr) && ((yr %% 4 == 0 & yr %% 100 != 0) | yr %% 400 == 0))
        last <- 29
    }
    gsub("\\blast\\b", last, s, ignore.case = TRUE, perl = TRUE)
  }, character(1), USE.NAMES = FALSE)
}

written_month <- function(dates) {
  dates <- stri_squish(stringi::stri_replace_all_regex(tolower(dates),
                                                        ",|-", " "))
  dates <- stringi::stri_replace_all_regex(dates,
                                    "([:alpha:]{3}) ([:digit:]{1,2}) ([:digit:]{4})",
                                    "$3 $1 $2")
  dates <- stringi::stri_replace_all_regex(dates,
                                    "([:digit:]{4}) ([:digit:]{1,2}) ([:alpha:]{3})",
                                    "$1 $3 $2")
  dates <- stringi::stri_replace_all_regex(dates, " jan ", "-01-")
  dates <- stringi::stri_replace_all_regex(dates, " feb ", "-02-")
  dates <- stringi::stri_replace_all_regex(dates, " mar ", "-03-")
  dates <- stringi::stri_replace_all_regex(dates, " apr ", "-04-")
  dates <- stringi::stri_replace_all_regex(dates, " may ", "-05-")
  dates <- stringi::stri_replace_all_regex(dates, " jun ", "-06-")
  dates <- stringi::stri_replace_all_regex(dates, " jul ", "-07-")
  dates <- stringi::stri_replace_all_regex(dates, " aug ", "-08-")
  dates <- stringi::stri_replace_all_regex(dates, " sep ", "-09-")
  dates <- stringi::stri_replace_all_regex(dates, " oct ", "-10-")
  dates <- stringi::stri_replace_all_regex(dates, " nov ", "-11-")
  dates <- stringi::stri_replace_all_regex(dates, " dec ", "-12-")
  # 6 digit my or ym dates
  dates <- stringi::stri_replace_all_regex(dates,
                                    "([:alpha:]{3}) ([:digit:]{4})",
                                    "$2 $1")
  dates <- stringi::stri_replace_all_regex(dates, " jan$", "-01")
  dates <- stringi::stri_replace_all_regex(dates, " feb$", "-02")
  dates <- stringi::stri_replace_all_regex(dates, " mar$", "-03")
  dates <- stringi::stri_replace_all_regex(dates, " apr$", "-04")
  dates <- stringi::stri_replace_all_regex(dates, " may$", "-05")
  dates <- stringi::stri_replace_all_regex(dates, " jun$", "-06")
  dates <- stringi::stri_replace_all_regex(dates, " jul$", "-07")
  dates <- stringi::stri_replace_all_regex(dates, " aug$", "-08")
  dates <- stringi::stri_replace_all_regex(dates, " sep$", "-09")
  dates <- stringi::stri_replace_all_regex(dates, " oct$", "-10")
  dates <- stringi::stri_replace_all_regex(dates, " nov$", "-11")
  dates <- stringi::stri_replace_all_regex(dates, " dec$", "-12")
  dates
}

reorder_ambiguous <- function(d) {
  examples <- ifelse(as.numeric(gsub("-", "", stringi::stri_extract_first_regex(d, "^[:digit:]{2}-"))) < 32 &
                       as.numeric(gsub("-", "", stringi::stri_extract_first_regex(d, "-[:digit:]{2}-"))) < 32 &
                       as.numeric(gsub("-", "", stringi::stri_extract_first_regex(d, "-[:digit:]{2}$"))) < 32,
                     d, NA_character_)
  input <- utils::menu(c("YMD (Year-Month-Day)", "DMY (Day-Month-Year)",
                         "MDY (Month-Day-Year)", "Ambiguous/Not sure"),
                       title = paste0("What is the component order of ambiguous 6 digit dates in vector
                                      (e.g. ", examples[stats::complete.cases(examples)][1], ")?"))
  if (input == 1) {
    out <- d
    message("Ambiguous 6 digit dates already in standard YMD format")
  }
  if (input == 2) {
    out <- stringi::stri_replace_all_regex(d, "^([:digit:]{2})-([:digit:]{2})-([:digit:]{2})$", "$3-$2-$1")
    message("Ambiguous 6 digit dates have been changed to standard YMD format")
  }
  if (input == 3) {
    out <- stringi::stri_replace_all_regex(d, "^([:digit:]{2})-([:digit:]{2})-([:digit:]{2})$", "$3-$1-$2")
    message("Ambiguous 6 digit dates have been changed to standard YMD format")
  }
  if (input == 4) {
    out <- stringi::stri_replace_all_regex(d, "^([:digit:]{2})-([:digit:]{2})-([:digit:]{2})$",
                                    "$1-$2-$3,$3-$2-$1,$3-$1-$2")
    message("Ambiguous 6 digit dates have been changed to a set of possible dates")
  }
  out
}

complete_ambiguous_20 <- function(d) {
  examples <- ifelse(as.numeric(gsub("-", "", stringi::stri_extract_first_regex(d, "^[:digit:]{2}-"))) < 23, d, NA_character_)
  examples <- examples[stats::complete.cases(examples)][1]
  input <- utils::menu(c("Yes", "No"),
                       title = paste0("Are all ambiguous 6 digit dates for which the year is between 0 and 23
                       in the 21st century (e.g. ", examples, " is equal to 20", examples, ")?"))
  if (input == 1) {
    out <- stringi::stri_replace_all_regex(d, "^([:digit:]{2})-([:digit:]{2})-([:digit:]{2})$", "20$1-$2-$3")
    message("Ambiguous 6 digit dates for which the year is smaller than 23 were completed.")
  }
  if (input == 2) {
    out <- d
    message("No changes were made to ambiguous 6 digit dates for which the year is smaller than 23.")
  }
  out
}

complete_ambiguous_19 <- function(d) {
  examples <- ifelse(as.numeric(gsub("-", "", stringi::stri_extract_first_regex(d, "^[:digit:]{2}-"))) > 22, d, NA_character_)
  examples <- examples[stats::complete.cases(examples)][1]
  input <- utils::menu(c("Yes", "No"),
                       title = paste0("Are all ambiguous 6 digit dates for which the year is larger than 22
                       in the 20th century (e.g. ", examples, " is equal to 19", examples, ")?"))
  if (input == 1) {
    out <- stringi::stri_replace_all_regex(d, "^([:digit:]{2})-([:digit:]{2})-([:digit:]{2})$", "19$1-$2-$3")
    message("6 digit dates for which the year is larger than 22 were completed.")
  }
  if (input == 2) {
    out <- d
    message("No changes were made to 6 digit dates for which the year is larger than 22.")
  }
  out
}

stri_squish <- function(charvec){
  stringi::stri_trim_both(stringi::stri_replace_all_regex(charvec, "\\s+", " "))
}
