#' A flexible date class for messy dates
#' @description
#'   Recent extensions to standardised date notation in
#'   [ISO 8601-2_2019(E)](https://www.iso.org/standard/70908.html)
#'   enable the recording of dates as unspecified, uncertain, approximate,
#'   and as a set or a range.
#'   These functions create and validate a new date class for R that can contain
#'   and parse these annotations.
#'   Whereas `new_messydate()` creates a new `mdate` object,
#'   and `make_messydate()` creates a new `mdate` object from one, two, or three date variables,
#'   `validate_messydate()` checks that the object is valid.
#'
#'   Note that the functions documented here are typically used internally, not by users;
#'   users are recommended to use `as_messydate()` to coerce dates and character strings,
#'   including historical prose, into `mdate` objects.
#' @section Dates and times:
#'   Since v1.0.0, messydates operates with and on both dates and times.
#'   Times of day may be appended to a date using a space,
#'   e.g. `2019-03-01 14:30:00`. ISO 8601-1 sec. 4.3.2 and RFC 3339 both
#'   permit a space as an alternative to the `T` separator more commonly
#'   seen in machine-generated timestamps; messydates uses a space for
#'   readability, though `T` continues to be accepted on input. Hours,
#'   minutes, and seconds are accepted (with optional fractional seconds),
#'   as are 12-hour `am`/`pm` times. Coordinated Universal Time is written
#'   with the `Z` designator, and other zones as a numeric offset,
#'   e.g. `+02:00`.
#'   Time components accept the same annotations as dates: approximate (`~`),
#'   uncertain (`?`), both (`%`), and unspecified (`X`),
#'   e.g. `2019-03-01 ~14:30` or `2019-03-01 14:XX`.
#'   Because `:` is also used as a range separator, times are detected and
#'   protected before ranges are parsed, so `2009-01-01:2019-01-01` remains a
#'   range while `2019-03-01 14:30:00` is read as a time.
#'   A time of day may also be given on its own, with no date part, e.g.
#'   `14:30` or `2:30pm`. This requires a clear time signal (a colon-separated
#'   clock or an `am`/`pm` suffix), so a bare number such as `2019` is still
#'   read as a year rather than an hour; a bare `am`/`pm` hour (`2pm`) is taken
#'   as an exact hour and filled to `14:00`.
#'
#' @section Imprecision annotations:
#'   _Unspecified date components_, such as when the day is unknown,
#'   can be represented by one or more `X`s in place of the digits.
#'   The modifier `*` is recommended to indicate that the entire
#'   time scale component value is unspecified, e.g. `X*-03-03`,
#'   however this is not implemented here.
#'   Please be explicit about the digits that are unspecified,
#'   e.g. `XXXX-03-03` expresses 3rd March in some unspecified year,
#'   whereas `2003-XX-03` expresses the 3rd of some month in 2003.
#'   If time components are not given, they are expanded to this.
#'
#'   _Approximate date components_, modified by `~`,
#'   represent an estimate whose value is asserted
#'   to be possibly correct.
#'   For example, `2003~-03-03`
#'   The degree of confidence in approximation
#'   depends on the application.
#'
#'   _Uncertain date components_, modified by `?`,
#'   represent a date component whose source is considered
#'   to be dubious and therefore not to be relied upon.
#'   An additional modifier, `%`, is used to indicate
#'   a value that is both uncertain and approximate.
#'
#' @section Set annotations:
#'   These functions also introduce standard notation for ranges of dates.
#'   Rather than the typical R notation for ranges, `:`,
#'   ISO 8601-2_2019(E) recommends `..`.
#'   This then can be applied between two time scale components to create a
#'   standard range between these dates (inclusive), e.g. `2009-01-01..2019-01-01`.
#'   But it can also be used as an affix,
#'   indicating "on or before" if used as a prefix, e.g. `..2019-01-01`,
#'   or indicating "on or after" if used as a suffix, e.g. `2009-01-01..`.
#'
#'   And lastly, notation for sets of dates is also included.
#'   Here braces, `{}`, are used to mean "all members of the set",
#'   while brackets, `[]`, are used to mean "one member of the set".
#' @param x A character scalar or vector in the expected `"yyyy-mm-dd"` format
#'   annotated, as necessary, according to ISO 8601-2_2019(E).
#' @return Object of class `mdate`
#' @name class_mdate
#' @seealso `as_messydate()` for the full, user-facing coercion pipeline,
#'   including parsing of free text and historical prose (e.g. Roman
#'   numerals, "circa", "between ... and ...").
#' @examples
#' new_messydate("2012-03-03")
#' validate_messydate(new_messydate(c("2012-03-03", "2012-XX-03~")))
#' # invalid characters or missing digits raise an error
#' tryCatch(validate_messydate(new_messydate("2012-03-03g")),
#'          error = function(e) e$message)
NULL

#' @rdname class_mdate
#' @export
new_messydate <- function(x = character()) {
  stopifnot(is.character(x))
  structure(x, class = "mdate")
}

#' @rdname class_mdate
#' @export
validate_messydate <- function(x) {
  values <- unclass(x)
  # 'X' marks unspecified components; 'T' is the ISO date-time separator and
  # 'Z' the UTC designator. All other letters are disallowed.
  if (any(grepl("[A-SU-WYa-wyz]", values) & !grepl("^NA$", values))) {
    stop("The only alpha characters allowed in messy dates are 'X' for
      unspecified components, and 'T'/'Z' for times", call. = FALSE)
  }
  if (!any(grepl("[0-9]", values))) {
    stop("mdate object requires at least one specified date component.",
         call. = FALSE)
    }
  if (any(grepl("!|\\(|\\)|\\=|;|>|<|_|\\^|'|&|\\$|#", values))) {
    stop("mdate object can only consist of numbers and
      some special symbols: []{}..X%?~ and, for times, T:+Z.", call. = FALSE)
  }
  if (any(grepl("\\+", gsub("\\+[0-9]{2}:[0-9]{2}", "", values)))) {
    stop("'+' can only appear as part of a timezone offset (+HH:MM).",
         call. = FALSE)
  }
  x
}

#' @rdname class_mdate
#' @param ... One (yyyy-mm-dd), two (yyyy-mm-dd, yyyy-mm-dd),
#'   or three (yyyy, mm, dd) variables.
#' @inheritParams coerce_to
#' @details If three date variables are passed to `make_messydate()`,
#'   function will create a single date (yyyy-mm-dd) from it.
#'   If two date variables are passed to `make_messydate()`,
#'   function will create a range of dates from it (yyyy-mm-dd..yyyy-mm-dd).
#'   If one date variable is passed to `make_messydate()`,
#'   function defaults to `as_messydate()`.
#' @examples
#' make_messydate("2010", "10", "10")
#' @export
make_messydate <- function(..., resequence = FALSE) {
  dots <- list(...)
  if (length(dots) == 1) {
    dots <- do.call(as.character, dots)
    dates <- unlist(dots)
  } else if (length(dots) == 2) {
    dots <- lapply(dots, as.character)
    dates <- do.call(paste, c(dots, sep = ".."))
    dates <- gsub("NA..NA", "NA", dates)
  } else if (length(dots) == 3) {
    dots <- lapply(dots, as.character)
    dates <- do.call(paste, c(dots, sep = "-"))
    dates <- gsub("NA-NA-NA", "NA", dates)
  } else stop("make_messydate() takes one variable (yyyy-mm-dd),
  two variables (yyyy-mm-dd, yyyy-mm-dd), or three variables (yyyy, mm, dd).")
  as_messydate(dates, resequence)
}

#' @rdname class_mdate
#' @importFrom utils str
#' @export
print.mdate <- function(x, ...) {
  str(x)
}

#' @rdname class_mdate
#' @details
#'   `format()` returns the underlying ISO 8601-2 strings, so that `mdate`
#'   vectors render legibly when held in a `data.frame` or tibble column
#'   (which format their columns rather than printing them).
#' @export
format.mdate <- function(x, ...) {
  format(unclass(x), ...)
}
