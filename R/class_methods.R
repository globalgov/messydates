#' Vector methods for `mdate` objects
#' @description
#'   `mdate` and `mduration` objects are built on the
#'   [vctrs](https://vctrs.r-lib.org) package.
#'   They therefore subset, replace, concatenate, repeat, and deduplicate
#'   like ordinary vectors, and keep their class in `data.frame`s, tibbles,
#'   and `dplyr` or `tidyr` pipelines (e.g. `bind_rows()`, `unnest()`).
#'
#'   Character strings, `Date`, and `POSIXct` values combine with `mdate`
#'   objects and are converted to `mdate`.
#'   A replacement value that cannot be read as a date raises an error,
#'   instead of a silent `NA`.
#'   An `mdate` converts to a character vector without loss,
#'   but conversion to an exact class, such as `Date`,
#'   needs an explicit resolution, see `as.Date()`.
#' @param x,y Vectors to combine or convert.
#' @param to The type to convert to.
#' @param ... Passed on to other methods.
#' @return An `mdate` object, except where the method converts to
#'   another type.
#' @details
#'   `unique()` and `duplicated()` compare the annotated strings, not the
#'   sets of dates they expand to, so two values that could refer to the same
#'   date but are written differently (e.g. `"2012-01"` and
#'   `"2012-01-01..2012-01-31"`) are treated as distinct.
#' @name class_methods
#' @examples
#' d <- as_messydate(c("2012-01-01", "2012-02-01", "2012-03-01"))
#' d[2]
#' d[2] <- "2012-02-02"
#' c(d, as_messydate("2012-04-01"))
#' c(d, as.Date("2012-05-01"))
#' rep(d, 2)
#' unique(as_messydate(c("2012-01-01", "2012-01-01", "2012-02-01")))
#' data.frame(event = c("a", "b", "c"), date = d)
NULL

#' @importFrom vctrs vec_ptype2 vec_cast vec_ptype_abbr vec_ptype_full
NULL

# Type labels ---------------------------------------------------------------

#' @export
vec_ptype_abbr.mdate <- function(x, ...) "mdate"

#' @export
vec_ptype_full.mdate <- function(x, ...) "mdate"

#' @export
vec_ptype_abbr.mduration <- function(x, ...) "mdur"

#' @export
vec_ptype_full.mduration <- function(x, ...) "mduration"

# Common types --------------------------------------------------------------
# Any combination of mdate with character, Date, or POSIXct gives mdate,
# because mdate can hold all of them without loss.

#' @rdname class_methods
#' @export
vec_ptype2.mdate.mdate <- function(x, y, ...) new_messydate()

#' @export
vec_ptype2.mdate.character <- function(x, y, ...) new_messydate()

#' @export
vec_ptype2.character.mdate <- function(x, y, ...) new_messydate()

#' @export
vec_ptype2.mdate.Date <- function(x, y, ...) new_messydate()

#' @export
vec_ptype2.Date.mdate <- function(x, y, ...) new_messydate()

#' @export
vec_ptype2.mdate.POSIXct <- function(x, y, ...) new_messydate()

#' @export
vec_ptype2.POSIXct.mdate <- function(x, y, ...) new_messydate()

#' @export
vec_ptype2.mduration.mduration <- function(x, y, ...) new_messyduration()

# Casts ---------------------------------------------------------------------

#' @rdname class_methods
#' @export
vec_cast.mdate.mdate <- function(x, to, ...) x

#' @export
vec_cast.mdate.character <- function(x, to, ...) coerce_replacement(x)

#' @export
vec_cast.character.mdate <- function(x, to, ...) unclass_vctr(x)

#' @export
vec_cast.mdate.Date <- function(x, to, ...) {
  suppressMessages(as_messydate(x))
}

#' @export
vec_cast.mdate.POSIXct <- function(x, to, ...) {
  suppressMessages(as_messydate(x))
}

#' @export
vec_cast.mduration.mduration <- function(x, to, ...) x

#' @export
vec_cast.character.mduration <- function(x, to, ...) unclass_vctr(x)

# Helpers -------------------------------------------------------------------

# Strips the vctrs class but keeps names, giving a plain character vector.
unclass_vctr <- function(x) {
  out <- vctrs::vec_data(x)
  attributes(out) <- NULL
  names(out) <- names(x)
  out
}

# Coerces a replacement value, treating text that names no date as an error
# rather than the silent NA that as_messydate() would return: an assignment
# that cannot be honoured should not quietly blank out an element.
coerce_replacement <- function(value) {
  out <- suppressWarnings(suppressMessages(as_messydate(value)))
  bad <- which(is.na(out) & !is.na(value))
  if (length(bad) > 0) {
    stop("Replacement value", if (length(bad) > 1) "s" else "",
         " could not be parsed as a date:\n",
         report_elements(bad, as.character(value)[bad]), call. = FALSE)
  }
  validate_messydate(out)
}
