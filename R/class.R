#' A flexible date class for messy dates
#'
#' @description
#' Recent extensions to standardised date notation in
#' [ISO 8601-2_2019(E)](https://www.iso.org/standard/70908.html)
#' create space for unspecified, uncertain, and approximate dates,
#' as well as succinct representation of ranges of dates.
#' These functions create and validate a new date class for R
#' that can contain and parse these annotations,
#' and are not typically user-facing.
#' Please see `as_messydate()` for the user-facing coercion function.
#'
#' @details
#' ## Date annotations
#'
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
#' ## Date sets
#'
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
#' @name class
#' @seealso messydate
NULL

#' @rdname class
#' @export
new_messydate <- function(x = character()) {
  stopifnot(is.character(x))
  structure(x, class = "mdate")
}

#' @rdname class
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

#' @importFrom utils str
#' @export
print.mdate <- function(x, ...) {
  str(x)
}

#' @export
`[.mdate` <- function(x, ..., drop = TRUE) {
  as_messydate(NextMethod("[", unclass(x)))
}

#' @export
`[<-.mdate` <- function(x, i, ..., value) {
  value <- as_messydate(value)
  validate_messydate(value)
  as_messydate(NextMethod("[<-", unclass(x)))
}

#' @export
`[[.mdate` <- function(x, ...) {
  as_messydate(NextMethod("[[", unclass(x)))
}

#' @export
`[[<-.mdate` <- function(x, i, ..., value) {
  value <- as_messydate(value)
  validate_messydate(value)
  as_messydate(NextMethod("[[<-", unclass(x)))
}

#' @export
c.mdate <- function(...) {
  if(length(list(...)) == 1){
    unclass(list(...)[[1]])
  } else {
    vecs <- lapply(list(...), function(e) unclass(as_messydate(e)))
    x <- as_messydate(unlist(vecs))
    validate_messydate(x)
  }
}

#' @export
as.data.frame.mdate <- function(x, ...) {
  as.data.frame.vector(x, ...)
}

#' @export
rep.mdate <- function(x, ...) {
  as_messydate(NextMethod("rep", unclass(x)))
}

#' @export
as.list.mdate <- function(x, ...) {
  lapply(unclass(x), as_messydate)
}

#' @export
as.numeric.mdate <- function(x, ...) {
  as.numeric(as.Date(x))
}

#' Sequence method for messydates
#' @description
#'   This function provides a sequence (`seq()`) method for messydates.
#'   This can be used with ranges or unspecified dates,
#'   and is particularly useful for defining a sequence of dates
#'   before the common era or between eras.
#' @name messy-sequence
#' @param from A messydate or range.
#'   If 'from' is a range and 'to' is not specified,
#'   'from' will be the minimum of the range and 'to' will be maximum.
#' @param to A messydate.
#' @param by Increment of the sequence. By default "days".
#' @param ... Arguments passed to or from methods.
#' @examples
#' seq(mdate("-0001-12-20"), mdate("0001-01-10"))
#' @export
seq.mdate <- function(from, to, by = "days", ...) {

  if(missing(to) & !is_precise(from)){
    to <- max(from)
    from <- min(from)
  }

  # straight forward sequence
  if(!any(is_bce(c(from, to)))){
    seq(as.Date(from), as.Date(to), by = by)
  } else {

    fromp <- as.Date(stringi::stri_replace_first_regex(from, "^-", ""))
    # sequence before common era
    if(is_bce(to)){
      top <- as.Date(stringi::stri_replace_first_regex(to, "^-", ""))
      .neg_seqs(fromp, top, by = by)
    } else {
      # sequence between eras
      zero_padding(c(.neg_seqs(fromp, as.Date("0001-12-31"), by = by),
                   as.character(seq(as.Date("0001-01-01"), as.Date(to), by = by))))
      # zero_padding(c(rev(paste0("-", seq(as.Date("0001-01-01"), fromp, by = by))),
      #   as.character(seq(as.Date("0001-01-01"), as.Date(to), by = by))))
    }
  }
}

.neg_seqs <- function(fromp, top, by = "days"){
  if(year(fromp) == year(top)){
    zero_padding(paste0("-", seq(min(c(fromp, top)),
                                 max(c(fromp,top)), by = by)))
  } else {
    strt <- max(c(fromp, top))
    ends <- min(c(fromp, top))
    strt_yr <- year(strt)
    strt_sq <- seq(as.Date(strt), as.Date(paste0(strt_yr,"-12-31")), by = by)
    ends_yr <- year(ends)
    ends_sq <- seq(as.Date(paste0(ends_yr, "-01-01")), as.Date(ends), by = by)
    if(strt_yr - ends_yr > 1){
      mids_sq <- seq(as.Date(paste0(ends_yr+1, "-01-01")),
                     as.Date(paste0(strt_yr-1,"-12-31")), by = by)
      if(length(unique(year(mids_sq)))>1)
        mids_sq <- mids_sq[order(year(mids_sq), decreasing = TRUE)]
      zero_padding(paste0("-", c(strt_sq, mids_sq, ends_sq)))
    } else zero_padding(paste0("-", c(strt_sq, ends_sq)))
  }
}
