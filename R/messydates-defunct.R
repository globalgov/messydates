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

#' @describeIn defunct Deprecated on 2023-08-25.
#' @export
messyduration <- function(.data) {
  .Deprecated(new = "make_messyduration",
              package = "messydates")
}

#' @describeIn defunct Deprecated on 2026-07-09.
#' @export
as_approximate <- function(x, component = NULL) {
  .Deprecated(new = "messydates::approximate()",
              package = "messydates")
}

#' @describeIn defunct Deprecated on 2026-07-09.
#' @export
as_uncertain <- function(x, component = NULL) {
  .Deprecated(new = "messydates::uncertain()",
              package = "messydates")
}

# Soft deprecation: the old name still works and gives a message once in
# each session. The old names go in a later minor release.
.deprecated_env <- new.env(parent = emptyenv())
.soft_deprecate <- function(old, new) {
  if (isTRUE(.deprecated_env[[old]])) return(invisible())
  .deprecated_env[[old]] <- TRUE
  message("`", old, "` is deprecated as of messydates 1.2.0. ",
          "Use `", new, "` instead.\n",
          "This message is shown once in each session.")
  invisible()
}

#' @describeIn defunct Deprecated in messydates 1.2.0.
#'   Use `date_is_precise()`.
#' @param x,y `mdate` objects.
#' @export
is_precise <- function(x) {
  .soft_deprecate("is_precise()", "date_is_precise()")
  date_is_precise(x)
}

#' @describeIn defunct Deprecated in messydates 1.2.0.
#'   Use `date_is_uncertain()`.
#' @export
is_uncertain <- function(x) {
  .soft_deprecate("is_uncertain()", "date_is_uncertain()")
  date_is_uncertain(x)
}

#' @describeIn defunct Deprecated in messydates 1.2.0.
#'   Use `date_is_approximate()`.
#' @export
is_approximate <- function(x) {
  .soft_deprecate("is_approximate()", "date_is_approximate()")
  date_is_approximate(x)
}

#' @describeIn defunct Deprecated in messydates 1.2.0.
#'   Use `date_is_bce()`.
#' @export
is_bce <- function(x) {
  .soft_deprecate("is_bce()", "date_is_bce()")
  date_is_bce(x)
}

#' @describeIn defunct Deprecated in messydates 1.2.0.
#'   Use `date_is_similar()`.
#' @export
is_similar <- function(x, y) {
  .soft_deprecate("is_similar()", "date_is_similar()")
  date_is_similar(x, y)
}
