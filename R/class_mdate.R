#' A flexible date class for messy dates
#' @description
#'   Recent extensions to standardised date notation in
#'   [ISO 8601-2_2019(E)](https://www.iso.org/standard/70908.html)
#'   create space for unspecified, uncertain, and approximate dates,
#'   as well as succinct representation of ranges of dates.
#'   These functions create and validate a new date class for R
#'   that can contain and parse these annotations,
#'   and are not typically user-facing.
#'   Please see `as_messydate()` for the user-facing coercion function.
#' @section Date annotations:
#' _Unspecified date components_, such as when the day is unknown,
#' can be represented by one or more `X`s in place of the digits.
#' The modifier `*` is recommended to indicate that the entire
#' time scale component value is unspecified, e.g. `X*-03-03`,
#' however this is not implemented here.
#' Please be explicit about the digits that are unspecified,
#' e.g. `XXXX-03-03` expresses 3rd March in some unspecified year,
#' whereas `2003-XX-03` expresses the 3rd of some month in 2003.
#' If time components are not given, they are expanded to this.
#'
#' _Approximate date components_, modified by `~`,
#' represent an estimate whose value is asserted
#' to be possibly correct.
#' For example, `2003~-03-03`
#' The degree of confidence in approximation
#' depends on the application.
#'
#' _Uncertain date components_, modified by `?`,
#' represent a date component whose source is considered
#' to be dubious and therefore not to be relied upon.
#' An additional modifier, `%`, is used to indicate
#' a value that is both uncertain and approximate.
#'
#' @section Date sets:
#' These functions also introduce standard notation
#' for ranges of dates.
#' Rather than the typical R notation for ranges,
#' `:`, ISO 8601-2_2019(E) recommends `..`.
#' This then can be applied between two time scale
#' components to create a standard range between
#' these dates (inclusive), e.g. `2009-01-01..2019-01-01`.
#' But it can also be used as an affix,
#' indicating "on or before" if used as a prefix,
#' e.g. `..2019-01-01`,
#' or indicating "on or after" if used as a suffix,
#' e.g. `2009-01-01..`.
#'
#' And lastly, notation for sets of dates is also included.
#' Here braces, `{}`, are used to mean "all members of the set",
#' while brackets, `[]`, are used to mean "one member of the set".
#' @param x A character scalar or vector in the expected `"yyyy-mm-dd"` format
#' annotated, as necessary, according to ISO 8601-2_2019(E).
#' @return Object of class `mdate`
#' @name class_create
#' @seealso messydate
NULL

#' @rdname class_create
#' @export
new_messydate <- function(x = character()) {
  stopifnot(is.character(x))
  structure(x, class = "mdate")
}

#' @rdname class_create
#' @export
validate_messydate <- function(x) {
  values <- unclass(x)
  if (any(grepl("[A-WYZa-z]", values) & !grepl("^NA$", values))) {
    stop("The only alpha character allowed in messy dates is 'X' for
      unspecified time components", call. = FALSE)
  }
  if (!any(grepl("[0-9]", values))) {
    stop("mdate object requires at least one specified date component.",
         call. = FALSE)
    }
  if (any(grepl("!|\\(|\\)|\\+|\\=|\\/|,|;|>|<|_|\\^|'|&|\\$|#", values))) {
    stop("mdate object can only consist of numbers and
      some special symbols: []{}..X%?~", call. = FALSE)
  }
  x
}

# Make ####

#' Composes `mdate` from multiple variables
#' @param ... One (yyyy-mm-dd), two (yyyy-mm-dd, yyyy-mm-dd),
#' or three (yyyy, mm, dd) variables.
#' @details If three date variables are passed to `make_messydate()`,
#' function will create a single date (yyyy-mm-dd) from it.
#' If two date variables are passed to `make_messydate()`,
#' function will create a range of dates from it (yyyy-mm-dd..yyyy-mm-dd).
#' If one date variable is passed to `make_messydate()`,
#' function defaults to `as_messydate()`.
#' @importFrom purrr map pmap_chr
#' @name class_make
#' @examples
#' make_messydate("2010", "10", "10")
#' @export
make_messydate <- function(..., resequence = FALSE) {
  dots <- list(...)
  if (length(dots) == 1) {
    dots <- do.call(as.character, dots)
    dates <- unlist(dots)
  } else if (length(dots) == 2) {
    dots <- purrr::map(dots, as.character)
    dates <- unlist(purrr::pmap_chr(dots, paste, sep = ".."))
    dates <- gsub("NA..NA", "NA", dates)
  } else if (length(dots) == 3) {
    dots <- purrr::map(dots, as.character)
    dates <- unlist(purrr::pmap_chr(dots, paste, sep = "-"))
    dates <- gsub("NA-NA-NA", "NA", dates)
  } else stop("make_messydate() takes one variable (yyyy-mm-dd),
  two variables (yyyy-mm-dd, yyyy-mm-dd), or three variables (yyyy, mm, dd).")
  as_messydate(dates, resequence)
}
