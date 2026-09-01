# Input validation and diagnostics ####

# ISO 8601-2 forms that this package does not (yet) represent. They are
# rejected at coercion rather than passed through, since an mdate holding one
# of these strings cannot be expanded, resolved, or compared.
# Ordinal dates and season codes are read in prose instead ("123rd day of
# 2019", "spring 2019"), the notations themselves being too easily confused
# with a plain date; see interpret_one() in R/coerce_to_messydate.R.
# Note that this concerns notation only. Durations, for instance, are very
# much part of the package: they are written as date ranges and handled by the
# 'mduration' class, which is what allows either end of one to be imprecise.
# It is the 'P1Y2M' way of writing them that is not read here.
.unsupported_forms <- list(
  list(pattern = "^-?[0-9]{4}-[0-9]{3}$",
       reason = paste("ordinal dates (e.g. '2019-123') are not supported,",
                      "as they are easily read as a mistake for '2019-12-03';",
                      "write '123rd day of 2019' instead")),
  list(pattern = "^-?[0-9]{4}-(2[1-9]|3[0-9]|4[01])$",
       reason = paste("season codes (e.g. '2019-21') are not supported,",
                      "as they are easily read as a mistake for '2019-02-01';",
                      "write 'spring 2019', or a month range, instead")),
  list(pattern = "^Y?-?[0-9]+S[0-9]+$",
       reason = "significant digits (e.g. '1234S3') are not supported"),
  list(pattern = "^Y-?[0-9]+E[0-9]+$",
       reason = "extended years (e.g. 'Y17E7') are not supported"),
  list(pattern = "^P([0-9]+[YMWD]|T[0-9]+)",
       reason = paste("ISO duration notation (e.g. 'P1Y2M') is not supported;",
                      "durations are written as date ranges instead, see",
                      "make_messyduration()")),
  list(pattern = "^R[0-9]*/",
       reason = paste("repeating intervals (e.g. 'R5/2019-01-01/P1Y') are",
                      "not supported"))
)

# Returns, per element, the reason it names an unsupported form, or NA.
unsupported_reason <- function(x) {
  x <- stringi::stri_trim_both(as.character(x))
  out <- rep(NA_character_, length(x))
  for (form in .unsupported_forms) {
    hit <- is.na(out) & !is.na(x) &
      stringi::stri_detect_regex(x, form$pattern)
    out[hit] <- form$reason
  }
  out
}

# Formats up to `n` offending elements as "index: value" for error messages.
report_elements <- function(index, value, n = 5) {
  shown <- utils::head(seq_along(index), n)
  more <- length(index) - length(shown)
  paste0(paste0("  [", index[shown], "] ", value[shown], collapse = "\n"),
         if (more > 0) paste0("\n  ... and ", more, " more"))
}

reject_unsupported <- function(x) {
  why <- unsupported_reason(x)
  bad <- which(!is.na(why))
  if (length(bad) == 0) return(invisible(TRUE))
  forms <- unique(why[bad])
  stop("Unsupported date format", if (length(forms) > 1) "s" else "", ":\n",
       paste0("  ", forms, collapse = "\n"), "\nin:\n",
       report_elements(bad, x[bad]), call. = FALSE)
}

# Returns, per element, the reason a date component falls outside its possible
# range, or NA. Operates on canonical mdate strings, so ranges and sets are
# checked member by member.
component_reason <- function(dates) {
  out <- rep(NA_character_, length(dates))
  parts <- stringi::stri_split_regex(dates, ",|\\.\\.")
  idx <- rep(seq_along(parts), lengths(parts))
  p <- unlist(parts, use.names = FALSE)
  p <- stringi::stri_replace_all_regex(p, "[~?%{}\\[\\]]", "")
  p <- stringi::stri_trim_both(p)
  keep <- nzchar(p) & !is.na(p)
  idx <- idx[keep]
  p <- p[keep]
  if (length(p) == 0) return(out)
  bare <- stringi::stri_detect_regex(p, "^[0-9X]{1,2}:")
  dpart <- ifelse(bare, "", stringi::stri_replace_first_regex(p, "[T ].*$", ""))
  tpart <- ifelse(bare, p,
                  ifelse(stringi::stri_detect_regex(p, "[T ]"),
                         stringi::stri_replace_first_regex(p, "^[^T ]*[T ]", ""),
                         NA_character_))
  rsn <- rep(NA_character_, length(p))
  # Date components
  dm <- stringi::stri_match_first_regex(
    dpart, "^(-?[0-9]{1,4})(?:-([0-9X]{2}))?(?:-([0-9X]{2}))?$")
  yr <- suppressWarnings(as.integer(dm[, 2]))
  mn <- suppressWarnings(as.integer(dm[, 3]))
  dy <- suppressWarnings(as.integer(dm[, 4]))
  bad <- !is.na(mn) & (mn < 1 | mn > 12)
  rsn[bad] <- paste0("'", dpart[bad], "' has month '", dm[bad, 3],
                     "', which is not between 01 and 12")
  # An unspecified month leaves any day up to 31 possible.
  maxday <- ifelse(is.na(mn) | mn < 1 | mn > 12, 31L, days_in(yr, mn))
  bad <- is.na(rsn) & !is.na(dy) & (dy < 1 | dy > maxday)
  rsn[bad] <- paste0("'", dpart[bad], "' has day '", dm[bad, 4],
                     "', but that month has only ", maxday[bad], " days")
  # Time components
  tm <- stringi::stri_match_first_regex(
    tpart, "^([0-9X]{2})(?::([0-9X]{2}))?(?::([0-9X]{2}))?")
  lim <- c(23L, 59L, 60L)
  unit <- c("hour", "minute", "second")
  for (k in 1:3) {
    v <- suppressWarnings(as.integer(tm[, k + 1]))
    bad <- is.na(rsn) & !is.na(v) & v > lim[k]
    rsn[bad] <- paste0("'", p[bad], "' has ", unit[k], " '", tm[bad, k + 1],
                       "', which is greater than ", lim[k])
  }
  hit <- !is.na(rsn)
  if (any(hit)) {
    first <- !duplicated(idx[hit])
    out[idx[hit][first]] <- rsn[hit][first]
  }
  out
}

check_components <- function(dates, source = dates) {
  why <- component_reason(dates)
  bad <- which(!is.na(why))
  if (length(bad) == 0) return(invisible(TRUE))
  stop("Impossible date component", if (length(bad) > 1) "s" else "", ":\n",
       paste0("  [", bad, "] ", source[bad], ": ", why[bad], collapse = "\n"),
       call. = FALSE)
}

#' Diagnose unparseable dates
#' @description
#'   Reports which elements of a vector `as_messydate()` cannot represent, and
#'   why. Where `as_messydate()` stops at the first problem, or returns `NA`
#'   for text it could not read, `md_problems()` inspects every element and
#'   returns one row per failure. This is intended for checking a column of
#'   dates before coercing it.
#' @param x A vector of dates, usually character.
#' @return A data frame with one row per problematic element and the columns
#'   `index` (its position in `x`), `input` (the offending value), and
#'   `reason`. A zero-row data frame means every element can be coerced.
#' @examples
#' md_problems(c("2019-01-01", "2019-02-30", "2019-W12", "not a date"))
#' # a clean vector returns no rows
#' md_problems(c("2019-01-01", "2019-01"))
#' @export
md_problems <- function(x) {
  x <- as.character(x)
  none <- data.frame(index = integer(), input = character(),
                     reason = character(), stringsAsFactors = FALSE)
  if (length(x) == 0) return(none)
  reason <- unsupported_reason(x)
  # Anything not already ruled out is coerced on its own, so that one bad
  # element neither hides nor is hidden by another.
  todo <- which(is.na(reason) & !is.na(x) & nzchar(stringi::stri_trim_both(x)))
  for (i in todo) {
    res <- tryCatch(suppressWarnings(as_messydate(x[i])),
                    error = function(e) conditionMessage(e))
    if (is.character(res) && !inherits(res, "mdate")) {
      # Errors are raised for a whole vector; strip that framing so the row
      # carries only the reason for this element.
      res <- stringi::stri_replace_all_regex(res, "\n\\s*", " ")
      res <- stringi::stri_replace_first_regex(
        res, "^(Impossible date component|Unsupported date format)s?: ", "")
      res <- stringi::stri_replace_first_regex(res, "^\\[[0-9]+\\] .*?: ", "")
      res <- stringi::stri_replace_first_regex(res, " in: \\[[0-9]+\\] .*$", "")
      reason[i] <- res
    } else if (any(is.na(res))) {
      reason[i] <- "could not be parsed as a date"
    }
  }
  bad <- which(!is.na(reason))
  if (length(bad) == 0) return(none)
  data.frame(index = bad, input = x[bad], reason = reason[bad],
             row.names = NULL, stringsAsFactors = FALSE)
}
