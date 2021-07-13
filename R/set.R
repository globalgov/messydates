#' Set operations for messy dates
#'
#' Performs intersection (`md_intersect()`) and union (`md_union()`) on,
#' inter alia, messy date class objects.
#' For a more typical 'join' that retains all elements, even if duplicated,
#' please use `md_multiset`.
#' @name set
#' @param x,y,... Messy date or other class objects
#' @return A vector of the same mode for `intersect`, or a common mode for union.
NULL
#> NULL

#' @describeIn set Find intersection of sets of messy dates
#' @examples
#' md_intersect(as_messydate("2012-01-01..2012-01-20"),as_messydate("2012-01"))
#' @export
md_intersect <- function(...){
  x <- list(...)[[1]]
  y <- list(...)[[2]]
  x <- as.character(expand(x)[[1]])
  y <- as.character(expand(y)[[1]])
  intersect(x, y)
}

#' @describeIn set Find union of sets of messy dates
#' @examples
#' md_union(as_messydate("2012-01-01..2012-01-20"),as_messydate("2012-01"))
#' @export
md_union <- function(x, y){
  x <- as.character(expand(x)[[1]])
  y <- as.character(expand(y)[[1]])
  union(x, y)
}

#' @describeIn set Join two sets of messy dates
#' @examples
#' md_multiset(as_messydate("2012-01-01..2012-01-20"),as_messydate("2012-01"))
#' @export
md_multiset <- function(x, y){
  x <- as.character(expand(x)[[1]])
  y <- as.character(expand(y)[[1]])
  c(x, y)
}

