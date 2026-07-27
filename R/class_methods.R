#' Basic vector methods for `mdate` objects
#' @description
#'   These methods let `mdate` vectors behave like ordinary character vectors
#'   for subsetting, replacement, concatenation, and repetition,
#'   while ensuring the result remains a validated `mdate` object.
#' @param x An `mdate` object.
#' @param i,... Index or indices, as for the default methods; for `c()`,
#'   one or more objects to concatenate (coerced to `mdate` first).
#' @param drop Included for consistency with the default `[` method;
#'   has no effect since `mdate` objects are always vectors.
#' @param value A replacement value, coerced to `mdate` and validated
#'   before assignment.
#' @return An `mdate` object, except for `c()`, which returns the (unclassed)
#'   result when called on a single object.
#' @name class_methods
#' @examples
#' d <- as_messydate(c("2012-01-01", "2012-02-01", "2012-03-01"))
#' d[2]
#' d[2] <- "2012-02-02"
#' c(d, as_messydate("2012-04-01"))
#' rep(d, 2)
NULL

#' @rdname class_methods
#' @export
`[.mdate` <- function(x, ..., drop = TRUE) {
  as_messydate(NextMethod("[", unclass(x)))
}

#' @rdname class_methods
#' @export
`[<-.mdate` <- function(x, i, ..., value) {
  value <- coerce_replacement(value)
  as_messydate(NextMethod("[<-", unclass(x)))
}

# Coerces a replacement value, treating text that names no date as an error
# rather than the silent NA that as_messydate() would return: an assignment
# that cannot be honoured should not quietly blank out an element.
coerce_replacement <- function(value) {
  out <- suppressWarnings(as_messydate(value))
  bad <- which(is.na(out) & !is.na(value))
  if (length(bad) > 0) {
    stop("Replacement value", if (length(bad) > 1) "s" else "",
         " could not be parsed as a date:\n",
         report_elements(bad, as.character(value)[bad]), call. = FALSE)
  }
  validate_messydate(out)
}

#' @rdname class_methods
#' @export
`[[.mdate` <- function(x, ...) {
  as_messydate(NextMethod("[[", unclass(x)))
}

#' @rdname class_methods
#' @export
`[[<-.mdate` <- function(x, i, ..., value) {
  value <- coerce_replacement(value)
  as_messydate(NextMethod("[[<-", unclass(x)))
}

#' @rdname class_methods
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

#' @rdname class_methods
#' @export
rep.mdate <- function(x, ...) {
  as_messydate(NextMethod("rep", unclass(x)))
}

