# Parsing text into ISO 8601-2 notation ####
# These helpers turn the text that `as_messydate()` receives into the
# canonical notation: two-digit years, separators, the order of the
# components, unspecified components, times of day, eras, zero padding,
# and English prose. Prose in other languages is first rewritten into
# English in `parse_languages.R`.

## Two-digit years ####
check_pivot <- function(pivot) {
  if (is.null(pivot)) return(invisible(TRUE))
  if (!is.numeric(pivot) || length(pivot) != 1 || is.na(pivot) ||
      pivot != round(pivot) || pivot < 100 || pivot > 9899)
    stop("`pivot` must be NULL or a single four-digit year, e.g. 1930.",
         call. = FALSE)
  invisible(TRUE)
}

# A two-digit year names a year within some century, but not which one.
# Without a pivot the century is left unspecified ("XX29"); with one, the
# year is placed in the 100-year window that starts at the pivot.
expand_two_digit_year <- function(yy, pivot) {
  if (is.null(pivot)) return(paste0("XX", yy))
  y <- (pivot %/% 100) * 100 + as.integer(yy)
  y <- ifelse(y < pivot, y + 100, y)
  sprintf("%04d", as.integer(y))
}

# After standardise_date_order(), a date whose year has only two digits
# has the form "yy-mm-dd". Those marked `keep` were written that way.
two_digit_years <- function(dates, pivot, keep = FALSE) {
  rx <- "^([0-9]{2})-([0-9]{2})-([0-9]{2})$"
  hit <- !is.na(dates) & stringi::stri_detect_regex(dates, rx) & !keep
  if (any(hit)) {
    m <- stringi::stri_match_first_regex(dates[hit], rx)
    dates[hit] <- paste0(expand_two_digit_year(m[, 2], pivot), "-",
                         m[, 3], "-", m[, 4])
  }
  dates
}

# With slashes and a two-digit last component ("1/1/29", "12/31/99"), the
# year is almost always last. Read it so, as day-month-year unless the
# middle component cannot be a month, and return "yy-mm-dd" for
# two_digit_years() to complete.
slash_two_digit_year <- function(dates) {
  rx <- "^\\s*([0-9]{1,2})/([0-9]{1,2})/([0-9]{2})\\s*$"
  hit <- !is.na(dates) & stringi::stri_detect_regex(dates, rx)
  if (!any(hit)) return(list(dates = dates, hit = hit))
  m <- stringi::stri_match_first_regex(dates[hit], rx)
  a <- as.integer(m[, 2]); b <- as.integer(m[, 3])
  mdy <- b > 12 & a <= 12
  day <- ifelse(mdy, b, a); mon <- ifelse(mdy, a, b)
  dates[hit] <- sprintf("%s-%02d-%02d", m[, 4], mon, day)
  list(dates = dates, hit = hit)
}

# A month name followed by a two-digit year, e.g. "May-97" or "Mar 97".
# With a space, a number up to 31 could be a day ("Mar 15"), so only a
# hyphen, an apostrophe, or a number above 31 marks it as a year.
month_two_digit_year <- function(x, pivot) {
  rx <- "^\\s*([A-Za-z]{3,9})\\.?(\\s*-\\s*|\\s+'?|')([0-9]{2})\\s*$"
  hit <- !is.na(x) & stringi::stri_detect_regex(x, rx)
  if (!any(hit)) return(x)
  m <- stringi::stri_match_first_regex(x[hit], rx)
  mn <- match(tolower(substr(m[, 2], 1, 3)), tolower(month.abb))
  full <- tolower(m[, 2]) %in% c(tolower(month.abb), tolower(month.name), "sept")
  yearish <- grepl("-|'", m[, 3]) | as.integer(m[, 4]) > 31
  ok <- !is.na(mn) & full & yearish
  idx <- which(hit)[ok]
  x[idx] <- paste0(expand_two_digit_year(m[ok, 4], pivot), "-",
                   sprintf("%02d", mn[ok]))
  x
}

#' @importFrom stringi stri_detect_regex
standardise_text <- function(v) {
  v <- convert_roman(v)
  # Drop ordinal suffixes on numeric days ("13th" -> "13") so both the text
  # extractor and the written-month parser see a plain number.
  v <- gsub("([0-9])(st|nd|rd|th)\\b", "\\1", v, ignore.case = TRUE, perl = TRUE)
  dates <- ifelse(stringi::stri_detect_regex(v, "([:alpha:]{4})") &
                    !grepl("bce$|^XXXX|XXXX$", v, ignore.case = TRUE),
                  extract_from_text(v), v)
  dates <- ifelse(grepl(paste(.month_abbr, collapse = "|"),
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
  times <- rep(list(character(0)), length(v))
  # Every time this function finds is marked by a 'T', a ':' or an am/pm, so
  # elements with none of these are left as they are without the loop below.
  maybe <- !is.na(v) & grepl("[T:]|[apAP][mM]", v)
  for (i in which(maybe)) {
    s <- v[i]
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
  ap <- .lower(regmatches(t, regexpr("[apAP][mM]$", t)))
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
  # Each step acts on one piece at a time, so all pieces are padded together
  # and then joined again into their own elements.
  parts <- strsplit(dates, "\\.\\.")
  x <- unlist(parts, use.names = FALSE)
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
  join_pieces(x, lengths(parts), "..")
}

# Joins `x`, split earlier into groups of sizes `n`, back into one string per
# group. A group with no pieces gives "", as paste() of nothing does.
join_pieces <- function(x, n, sep) {
  g <- factor(rep(seq_along(n), n), levels = seq_along(n))
  vapply(split(x, g), paste, character(1), collapse = sep, USE.NAMES = FALSE)
}

add_zero_set <- function(dates) {
  # Each step acts on one piece at a time, so all pieces are padded together
  # and then joined again into their own elements.
  parts <- strsplit(dates, "\\,")
  x <- unlist(parts, use.names = FALSE)
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
  join_pieces(x, lengths(parts), ",")
}

# Natural-language interpretation of historical date prose. Recognises Roman
# calendar references (Kalends/Nones/Ides), approximate or uncertain qualifiers
# ("around", "circa", "possibly"), and connectives implying a range
# ("between .. and .."), a set (".. or .."), or several dates (".. and ..").
# Strings without such cues are returned unchanged for the normal text parser.
interpret_prose <- function(x) {
  # Every cue interpret_one() reads is a word, so elements with no letter
  # (most ISO input) are returned as they are without the scalar parser.
  # interpret_one() gives the same result for the same string, so each
  # different string is interpreted once and the result is used again for
  # the repeats, which are common in a data column.
  prose <- stringi::stri_detect_regex(x, "\\p{L}")
  prose[is.na(prose)] <- FALSE
  out <- as.list(x)
  if (any(prose)) {
    u <- unique(x[prose])
    out[prose] <- lapply(u, interpret_one)[match(x[prose], u)]
  }
  unlist(out, use.names = FALSE)
}

# The English month names, in month order. Every month lookup in the parser
# is made from this one table.
.month_names <- c("january", "february", "march", "april", "may", "june",
                  "july", "august", "september", "october", "november",
                  "december")
.month_abbr <- substr(.month_names, 1, 3)

# Full names first, then the abbreviations ("sept" before "sep", and no "may",
# which is already a full name), so a regex alternation of these names finds
# the longest form.
.nl_months <- c(stats::setNames(1:12, .month_names),
                stats::setNames(c(1:4, 6:8, 9, 9:12),
                                c(.month_abbr[1:4], .month_abbr[6:8], "sept",
                                  .month_abbr[9:12])))
storage.mode(.nl_months) <- "double"

# Lower case by English rules, whatever the session locale. Under a Turkish
# locale base tolower() maps "I" to a dotless i, so "APRIL" would not match.
.lower <- function(x) stringi::stri_trans_tolower(x, locale = "en")

nl_month_num <- function(s) {
  m <- stringi::stri_extract_first_regex(
    .lower(s), paste(names(.nl_months), collapse = "|"))
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

# The annotation that the qualifier words in lower-case prose give:
# "~" for approximate ("around"), "?" for uncertain ("possibly"), "%" for
# both, or NA for none.
prose_qualifier <- function(low) {
  approx <- grepl(paste0("\\b(", .approx_words_rx, ")\\b"), low)
  uncert <- grepl(paste0("\\b(", .uncert_words_rx, ")\\b"), low)
  if (approx && uncert) "%" else if (approx) "~" else if (uncert) "?" else NA_character_
}

interpret_one <- function(s) {
  if (is.na(s) || !is.character(s)) return(s)
  low <- .lower(s)

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
  # A century before the common era keeps its era: the 5th century BC is
  # 500-401 BC, which is the astronomical years -0499 to -0400, "-04XX".
  cen <- stringi::stri_match_first_regex(low, "([0-9]+)(?:st|nd|rd|th)?\\s+centur")
  if (!is.na(cen[1, 1])) {
    bce <- grepl(paste0("\\b(", .bc_rx, ")\\b"), s)
    return(sprintf("%s%02dXX", if (bce) "-" else "",
                   as.integer(cen[1, 2]) - 1L))
  }

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
    # A qualifier applies to both days, on the day component, as for a
    # single day ("around the 13th and 15th of Feb" marks both days).
    q <- prose_qualifier(low)
    q <- if (is.na(q)) "" else q
    d1 <- sprintf("%s-%02d-%s%02d", yr, mn, q, as.integer(dl[1, 2]))
    d2 <- sprintf("%s-%02d-%s%02d", yr, mn, q, as.integer(dl[1, 4]))
    if (between || conn == "to") return(paste0(d1, "..", d2))
    if (conn == "or") return(paste0("{", d1, ",", d2, "}"))
    return(c(d1, d2)) # a plain "and" lists several separate dates
  }

  # 3. A single date carrying an approximate and/or uncertain qualifier
  # (both together give the EDTF "%" marker).
  approx_words <- .approx_words_rx
  uncert_words <- .uncert_words_rx
  qual <- prose_qualifier(low)
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
    # A range: the qualifier applies to the whole range, so it marks each
    # end, as a suffix on a date and as a prefix on a year
    # ("approximately 2019-01-01..2019-01-05" -> "2019-01-01~..2019-01-05~",
    # "circa 1990..1995" -> "~1990..~1995"). An open end stays open.
    if (!is.na(iso) && grepl("..", iso, fixed = TRUE) &&
        !grepl("[{}\\[\\],]", iso)) {
      ends <- stringi::stri_split_fixed(iso, "..")[[1]]
      ends <- vapply(ends, function(e) {
        if (!nzchar(e)) e
        else if (grepl("-", sub("^-", "", e))) paste0(e, qual)
        else paste0(qual, e)
      }, character(1), USE.NAMES = FALSE)
      return(paste0(paste(ends, collapse = ".."), era))
    }
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
    grepl(paste0("^(", paste(.month_abbr, collapse = "|"), ")"), first_tok,
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
  for (k in seq_along(.month_names)) {
    out <- gsub(.month_names[k], sprintf("-%02d-", k),
                out, ignore.case = TRUE, perl = TRUE)
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
  mon_last <- stats::setNames(c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31),
                              .month_abbr)
  vapply(v, function(s) {
    if (is.na(s) || !grepl("\\blast\\b", s, ignore.case = TRUE, perl = TRUE))
      return(s)
    m3 <- .lower(substr(stringi::stri_extract_first_regex(
      s, paste0("(?i)", paste(.month_abbr, collapse = "|"))), 1, 3))
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
  dates <- stri_squish(stringi::stri_replace_all_regex(.lower(dates),
                                                        ",|-", " "))
  dates <- stringi::stri_replace_all_regex(dates,
                                    "([:alpha:]{3}) ([:digit:]{1,2}) ([:digit:]{4})",
                                    "$3 $1 $2")
  dates <- stringi::stri_replace_all_regex(dates,
                                    "([:digit:]{4}) ([:digit:]{1,2}) ([:alpha:]{3})",
                                    "$1 $3 $2")
  for (k in seq_along(.month_abbr)) {
    dates <- stringi::stri_replace_all_regex(dates, paste0(" ", .month_abbr[k], " "),
                                             sprintf("-%02d-", k))
  }
  # 6 digit my or ym dates
  dates <- stringi::stri_replace_all_regex(dates,
                                    "([:alpha:]{3}) ([:digit:]{4})",
                                    "$2 $1")
  for (k in seq_along(.month_abbr)) {
    dates <- stringi::stri_replace_all_regex(dates, paste0(" ", .month_abbr[k], "$"),
                                             sprintf("-%02d", k))
  }
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
