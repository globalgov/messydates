#' Basic vector methods for `mdate` objects
#' @description
#'   These methods let `mdate` vectors behave like ordinary character vectors
#'   for subsetting, replacement, concatenation, repetition, and printing,
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
  value <- as_messydate(value)
  validate_messydate(value)
  as_messydate(NextMethod("[<-", unclass(x)))
}

#' @rdname class_methods
#' @export
`[[.mdate` <- function(x, ...) {
  as_messydate(NextMethod("[[", unclass(x)))
}

#' @rdname class_methods
#' @export
`[[<-.mdate` <- function(x, i, ..., value) {
  value <- as_messydate(value)
  validate_messydate(value)
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

# Printing ####

#' @rdname class_methods
#' @importFrom utils str
#' @export
print.mdate <- function(x, ...) {
  str(x)
}
#' @rdname class_methods
#' @importFrom utils str
#' @export
print.mdates_duration <- function(x, ...) {
  str(x)
}


