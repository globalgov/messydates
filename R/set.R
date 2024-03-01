#' Set operations for messy dates
#'
#' Performs intersection (`md_intersect()`) and union (`md_union()`) on,
#' inter alia, messy date class objects.
#' For a more typical 'join' that retains all elements, even if duplicated,
#' please use `md_multiset`.
#' @name set
#' @param e1,e2 Messy date or other class objects
#' @return A vector of the same mode for `intersect`,
#' or a common mode for union.
NULL

#' @rdname set
#' @export
`%intersect%` <- function(e1, e2) UseMethod("%intersect%")

#' @describeIn set Find intersection of sets of messy dates
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

#' @rdname set
#' @export
`%union%` <- function(e1, e2) UseMethod("%union%")

#' @describeIn set Find intersection of sets of messy dates
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
