#' Set operations for messy dates
#' @description
#'   Performs intersection (`%intersect%`) and union (`%union%`) on the
#'   dates or date-times implied by messy date class objects, treating each
#'   as the (day-granularity) set of dates it expands to. Both return a
#'   plain character vector of the individual member dates.
#'
#'   For a union that instead returns an `mdate` object in its most
#'   succinct (contracted) notation, e.g. a range rather than a list of
#'   every day within it, use `+` (see `?operate_arithmetic`) instead.
#' @name operate_set
#' @param e1,e2 Messy date or other class objects
#' @return A vector of the same mode for `%intersect%`,
#'   or a common mode for `%union%`.
NULL

#' @rdname operate_set
#' @export
`%intersect%` <- function(e1, e2) UseMethod("%intersect%")

#' @describeIn operate_set Find intersection of sets of messy dates
#' @examples
#' as_messydate("2012-01-01..2012-01-20") %intersect% as_messydate("2012-01")
#' @export
`%intersect%.mdate` <- function(e1, e2) {
  x <- as.character(expand(e1)[[1]])
  y <- as.character(expand(e2)[[1]])
  intersect(x, y)
}

evalqOnLoad({
  registerS3method("%intersect%", "Date", `%intersect%.mdate`)
  registerS3method("%intersect%", "POSIXt", `%intersect%.mdate`)
})

#' @rdname operate_set
#' @export
`%union%` <- function(e1, e2) UseMethod("%union%")

#' @describeIn operate_set Find union of sets of messy dates
#' @examples
#' as_messydate("2012-01-01..2012-01-20") %union% as_messydate("2012-01")
#' @export
`%union%.mdate` <- function(e1, e2) {
  x <- as.character(expand(e1)[[1]])
  y <- as.character(expand(e2)[[1]])
  union(x, y)
}

evalqOnLoad({
  registerS3method("%union%", "Date", `%union%.mdate`)
  registerS3method("%union%", "POSIXt", `%union%.mdate`)
})
