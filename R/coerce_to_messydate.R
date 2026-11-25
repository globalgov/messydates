#' Coercion from common date classes to `mdate`
#' @description
#'   These methods coerce various date classes into the `mdate` class.
#'   They represent the main user-facing class-creating functions in the package.
#'   In addition to the typical date classes in R (`Date`, `POSIXct`, and `POSIXlt`),
#'   there is also a direct method for converting text or character strings to `mdate`.
#'   The function can also extract dates and times from text,
#'   including some historical prose conventions,
#'   in English or French (see `lang`).
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
#'   A two-digit year, in any of these orders, does not say which century
#'   it belongs to; see `pivot`.
#' @param pivot For character input, how to read two-digit years, such as
#'   the "29" in "1/1/29" or the "97" in "May-97".
#'   By default (`NULL`) the century is left unspecified,
#'   so "1/1/29" becomes `"XX29-01-01"`, and no century is invented.
#'   Alternatively, a four-digit year that starts a 100-year window:
#'   two-digit years are placed in that window.
#'   For example, `pivot = 1930` reads "29" as 2029 and "30" as 1930,
#'   which is what Excel for Windows does by default,
#'   and `pivot = 1969` reproduces R's own `%y` convention.
#' @param origin For numeric input, the origin of a serial day count.
#'   Spreadsheets store dates as a count of days from an origin,
#'   and a file that loses its date formatting carries only these counts.
#'   Use `"excel"` for Excel's default (Windows) date system, which counts
#'   serial 1 as 1900-01-01 and, for compatibility with Lotus 1-2-3, also
#'   counts the non-existent 29 February 1900 as serial 60;
#'   use `"excel1904"` for the 1904 date system of older Excel for Mac;
#'   or give any date, such as `"1970-01-01"`, as the day that counts as zero.
#'   A fractional part is read as a time of day.
#'   By default (`NULL`), whole numbers from -9999 to 9999 are read as years,
#'   eight-digit numbers that form a valid date are read as YYYYMMDD,
#'   and any other number becomes `NA` with a warning,
#'   since without an origin it cannot be told apart from a serial count.
#' @param lang For character input, the language of any prose in `x`:
#'   `"en"` for English (the default) or `"fr"` for French.
#'   French prose is rewritten into the English forms described in
#'   "Parsing prose" below, so the same conventions are read in both,
#'   e.g. `"vers 1910"`, `"le XIXe siècle"`, `"les ides de mars, 44 av. J.-C."`,
#'   or `"entre le 13 et le 15 février 1977"`.
#'   Dates written only in digits are read the same in every language.
#' @param ... Arguments passed to methods.
#' @return A `mdate` class object
#' @family coerce
#' @section Unspecified components and one-member sets:
#'  An unspecified month or day is kept as `XX`: `"1993-01-XX"` says that
#'  the record has a day that is not known, while `"1993-01"` says only
#'  that it names the month. A month or day written as `0`, `00`, `??` or
#'  `NA` is read as `XX`. Both forms have the same possible dates, and the
#'  resolution functions give the same results for them.
#'
#'  A set with one member keeps its braces: `"{1993-01-01..1993-01-31}"`
#'  is all the days of January (a spell, such as a monthly count), and
#'  `"[1993-01-01..1993-01-31]"` is one of them. A bare range, and a range
#'  in prose ("from the 13th to the 15th of February"), is one of its days.
#'
#'  Functions that write a result from its days, such as `+`,
#'  `%intersect%`, and `as_messydate()` on a list, write the shortest form,
#'  without `XX` or braces.
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
#'  \item{French prose, with `lang = "fr"`. The same conventions are read,
#'  e.g. `"vers 1910"` becomes `"~1910"`, `"le XIXe siècle"` becomes
#'  `"18XX"`, and `"du 13 au 15 février 1977"` becomes
#'  `"1977-02-13..1977-02-15"`. Accents may be left out.
#'  Spelled-out numbers are read in English only.}
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
#' # two-digit years leave the century unspecified, unless a pivot is given
#' as_messydate(c("1/1/29", "May-97"))
#' as_messydate(c("1/1/29", "May-97"), pivot = 1930)
#' # serial day counts from spreadsheets need their origin
#' as_messydate(c(36892, 45000.5), origin = "excel")
#' as_messydate(c(35430, 1462), origin = "excel1904")
#' as_messydate(c(2021, 20210201))
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
#' # French prose
#' as_messydate(c("vers 1910", "le 15 mars 2019", "les ann\u00e9es 1990"), lang = "fr")
#' as_messydate("les ides de mars, 44 av. J.-C.", lang = "fr")
#' @export
as_messydate <- function(x, resequence = FALSE, ...)
  UseMethod("as_messydate")

#' @describeIn coerce_to Coerce from `Date` to `mdate` class
#' @export
as_messydate.Date <- function(x, resequence = FALSE, ...) {
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
as_messydate.POSIXct <- function(x, resequence = FALSE, ...) {
  new_messydate(posix_to_iso(x))
}

#' @describeIn coerce_to Coerce from `POSIXlt` to `mdate` class
#' @export
as_messydate.POSIXlt <- function(x, resequence = FALSE, ...) {
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
as_messydate.mdate <- function(x, resequence = FALSE, ...) {
  x <- as.character(x) # For updating 'mdate' variables
  new_messydate(x)
}

#' @describeIn coerce_to Coerce character date objects to `mdate` class
#' @export
as_messydate.character <- function(x, resequence = NULL, pivot = NULL,
                                   lang = "en", ...) {
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
  check_pivot(pivot)
  check_lang(lang)
  orig <- x
  # Prose in another language is rewritten into English prose first
  # (see R/parse_languages.R); English is left as it is.
  x <- translate_prose(x, lang)
  # A weekday name in front of a date ("Thu, 01 Sep 2016") adds nothing that
  # the date does not say, and its comma would split the value in two.
  x <- strip_weekday(x)
  # A month name with a two-digit year ("May-97", Excel's display of a month)
  # is read before the time parser can mistake the year for an hour.
  x <- month_two_digit_year(x, pivot)
  x <- interpret_prose(x)
  if (length(orig) != length(x)) orig <- x
  # '{}' is the empty set, a messydates extension to ISO 8601-2 (which
  # requires a set to have at least one member). It names no date components,
  # so it is set aside here and restored after the date pipeline.
  empty <- is_empty_notation(x)
  # A set with one member, such as "{1993-01-01..1993-01-31}" (all the days
  # of January, e.g. a monthly count) or "[1993-01-01..1993-01-31]" (one of
  # them), loses its braces below, as a set is rebuilt from its commas.
  # Note it here, so that the braces can be restored.
  curly1 <- !is.na(x) & stringi::stri_detect_regex(x, "^\\s*\\{[^,{}\\[\\]]+\\}\\s*$")
  square1 <- !is.na(x) & stringi::stri_detect_regex(x, "^\\s*\\[[^,{}\\[\\]]+\\]\\s*$")
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
  # A year written first ("39-01-01") is kept as written, as historical
  # dates may be; a two-digit year the parser had to move to the front was
  # written last, and its century is not known (see two_digit_years()).
  pre <- standardise_date_separators(d)
  slashed <- rep(FALSE, length(d))
  if (is.null(resequence)) {
    sl <- slash_two_digit_year(d)
    d <- sl$dates; slashed <- sl$hit
  }
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
  d <- two_digit_years(d, pivot, keep = !slashed & !is.na(d) & d == pre)
  d <- standardise_unspecifieds(d)
  d <- standardise_date_input(d)
  d <- standardise_widths(d)
  d <- restore_times(d, prot$times)
  if (any(onesie))
    d[onesie] <- stringi::stri_replace_all_regex(d[onesie], "^\\{(.*)\\}$", "[$1]")
  bare <- !is.na(d) & !stringi::stri_detect_regex(d, "^[{\\[]")
  if (any(curly1 & bare)) d[curly1 & bare] <- paste0("{", d[curly1 & bare], "}")
  if (any(square1 & bare)) d[square1 & bare] <- paste0("[", d[square1 & bare], "]")
  d[empty] <- "{}"
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
          "\nCall parse_problems() to see all parsing problems.", call. = FALSE)
}

#' @describeIn coerce_to Coerce factors to `mdate` class, via their labels.
#'   This is the common case when a date column has been read in with
#'   `stringsAsFactors = TRUE`.
#' @export
as_messydate.factor <- function(x, resequence = NULL, ...) {
  as_messydate(as.character(x), resequence, ...)
}

#' @export
as_messydate.default <- function(x, resequence = NULL, ...) {
  stop("Objects of class '", paste(class(x), collapse = "/"),
       "' cannot be coerced to 'mdate'. Convert to character, Date, or ",
       "POSIXct first.", call. = FALSE)
}

#' @describeIn coerce_to Coerce numeric objects to `mdate` class
#' @export
as_messydate.numeric <- function(x, resequence = NULL, origin = NULL, ...) {
  inf <- is.infinite(x)
  out <- rep(NA_character_, length(x))
  out[inf] <- ifelse(x[inf] > 0, "9999-12-31", "-9999-01-01")
  ok <- !is.na(x) & !inf
  if (!is.null(origin)) {
    out[ok] <- serial_to_iso(x[ok], origin)
    return(new_messydate(out))
  }
  whole <- ok & x == round(x)
  year <- whole & abs(x) <= 9999
  out[year] <- as.character(x[year])
  ymd <- whole & x >= 10000101 & x <= 99991231
  if (any(ymd)) {
    v <- sprintf("%08.0f", x[ymd])
    iso <- paste0(substr(v, 1, 4), "-", substr(v, 5, 6), "-", substr(v, 7, 8))
    valid <- !is.na(as.Date(iso, optional = TRUE))
    out[which(ymd)[valid]] <- iso[valid]
  }
  bad <- ok & is.na(out)
  if (any(bad))
    warning(sum(bad), " number", if (sum(bad) > 1) "s" else "",
            " could not be read as a year or as YYYYMMDD and became NA:\n",
            report_elements(which(bad), x[bad]),
            "\nIf these are serial day counts, such as dates from a spreadsheet, ",
            "give their origin, e.g. `origin = \"excel\"`.", call. = FALSE)
  new_messydate(out)
}

# Converts serial day counts to ISO 8601 dates (with a time of day where the
# count has a fractional part), given the origin of the count.
serial_to_iso <- function(x, origin) {
  if (!is.character(origin) && !inherits(origin, "Date"))
    stop("`origin` must be \"excel\", \"excel1904\", or a date.", call. = FALSE)
  day <- floor(x)
  frac <- x - day
  if (identical(origin, "excel")) {
    # Excel's default date system counts serial 1 as 1900-01-01 and treats
    # 1900 as a leap year, so serial 60 is a day that never existed and
    # serials from 61 on are one day ahead of a plain count.
    fake <- day == 60
    if (any(fake))
      warning(sum(fake), " serial", if (sum(fake) > 1) "s" else "",
              " equal to 60 became NA: in Excel's 1900 date system this is ",
              "29 February 1900, a day that never existed.", call. = FALSE)
    early <- day < 1
    if (any(early))
      warning(sum(early), " serial", if (sum(early) > 1) "s" else "",
              " below 1 became NA: Excel's 1900 date system starts at 1.",
              call. = FALSE)
    d <- as.Date(ifelse(day < 60, day - 1, day - 2), origin = "1900-01-01")
    d[fake | early] <- NA
  } else if (identical(origin, "excel1904")) {
    d <- as.Date(day, origin = "1904-01-01")
  } else {
    d <- as.Date(day, origin = as.Date(origin))
  }
  out <- format(d, "%Y-%m-%d")
  secs <- round(frac * 86400)
  timed <- !is.na(d) & secs > 0
  if (any(timed)) {
    secs[timed & secs >= 86400] <- 86399
    out[timed] <- paste0(out[timed], " ",
                         sprintf("%02d:%02d:%02d", secs[timed] %/% 3600,
                                 (secs[timed] %% 3600) %/% 60, secs[timed] %% 60))
  }
  out
}


#' @describeIn coerce_to Coerce a list of dates to the shortest `mdate`.
#'   Each element of the list gives one element of the result.
#'   This is the inverse of `compatible_dates()`.
#'   An empty element gives the empty set, `"{}"`,
#'   and an element that is only `NA` gives `NA`.
#' @param collapse For a list, should a range that covers a whole month or
#'   year become that month or year? `TRUE` by default, so that
#'   `"2012-06-01..2012-06-30"` becomes `"2012-06"`.
#' @examples
#' as_messydate(list(c("2012-06-01", "2012-06-02", "2012-06-03")))
#' as_messydate(list(c("2012-06-01", "2012-06-02", "2012-06-03",
#' "{2012-06-01, 2012-06-02, 2012-06-03}", "2012-06-01", "2012-06-03")))
#' # the shortest form of a messy date
#' as_messydate(compatible_dates(as_messydate("2012-06-01..2012-06-30")))
#' as_messydate(compatible_dates(as_messydate("2012-06-01..2012-06-30")),
#'   collapse = FALSE)
#' @export
as_messydate.list <- function(x, resequence = FALSE, collapse = TRUE, ...) {
  out <- vapply(x, function(y) {
    y <- as.character(y)
    if (length(y) == 0) return("{}")
    if (all(is.na(y))) return(NA_character_)
    y <- y[!is.na(y)]
    as.character(suppressMessages(.contract(paste(new_messydate(y),
                                                  collapse = ","),
                                            collapse = collapse)))
  }, character(1), USE.NAMES = FALSE)
  out <- new_messydate(out)
  names(out) <- names(x)
  out
}

#' @rdname coerce_to
#' @export
mdate <- as_messydate
