#' Functions that have been renamed, superseded, or are no longer working
#'
#' `r lifecycle::badge("deprecated")`
#' Generally these functions have been superseded or renamed.
#' Upon using them, a message is provided directing the user to the new function.
#' However, at this stage of package development,
#' we generally clear older defunct functions at each minor release,
#' and so you are strongly encouraged to use the new functions/names/syntax
#' wherever possible and update your scripts accordingly.
#' @name defunct
#' @keywords internal
NULL

#' @describeIn defunct Deprecated on 2023-08-25.
#' @export
is_element <- function(.data) {
  .Deprecated(new = "messydates::is_subset()",
              package = "messydates")
}

#' @describeIn defunct Deprecated on 2023-08-25.
#' @export
md_intersect <- function(.data) {
  .Deprecated(new = "messydates::`%intersect%`()",
              package = "messydates")
}

#' @describeIn defunct Deprecated on 2023-08-25.
#' @export
md_union <- function(.data) {
  .Deprecated(new = "messydates::`%union%`()",
              package = "messydates")
}

#' @describeIn defunct Deprecated on 2023-08-25.
#' @export
md_multiset <- function(.data) {
  .Deprecated(new = "messydates::`+`()",
              package = "messydates")
}
