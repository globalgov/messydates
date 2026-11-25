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

#' @describeIn defunct Deprecated on 2026-07-09. Removed in messydates 1.3.0.
#' @export
as_approximate <- function(x, component = NULL) {
  .Deprecated(new = "messydates::tag_approximate()",
              package = "messydates")
}

#' @describeIn defunct Deprecated on 2026-07-09. Removed in messydates 1.3.0.
#' @export
as_uncertain <- function(x, component = NULL) {
  .Deprecated(new = "messydates::tag_uncertain()",
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

#' @describeIn defunct Deprecated in messydates 1.2.0. Removed in 1.4.0.
#'   Use `date_is_precise()`.
#' @param x,y `mdate` objects.
#' @export
is_precise <- function(x) {
  .soft_deprecate("is_precise()", "date_is_precise()")
  date_is_precise(x)
}

#' @describeIn defunct Deprecated in messydates 1.2.0. Removed in 1.4.0.
#'   Use `date_is_uncertain()`.
#' @export
is_uncertain <- function(x) {
  .soft_deprecate("is_uncertain()", "date_is_uncertain()")
  date_is_uncertain(x)
}

#' @describeIn defunct Deprecated in messydates 1.2.0. Removed in 1.4.0.
#'   Use `date_is_approximate()`.
#' @export
is_approximate <- function(x) {
  .soft_deprecate("is_approximate()", "date_is_approximate()")
  date_is_approximate(x)
}

#' @describeIn defunct Deprecated in messydates 1.2.0. Removed in 1.4.0.
#'   Use `date_is_bce()`.
#' @export
is_bce <- function(x) {
  .soft_deprecate("is_bce()", "date_is_bce()")
  date_is_bce(x)
}

#' @describeIn defunct Deprecated in messydates 1.2.0. Removed in 1.4.0.
#'   Use `date_is_similar()`.
#' @export
is_similar <- function(x, y) {
  .soft_deprecate("is_similar()", "date_is_similar()")
  date_is_similar(x, y)
}

#' @describeIn defunct Deprecated in messydates 1.2.0. Removed in 1.4.0.
#'   Use `tag_approximate()`.
#' @param component The component to tag. See [parse_tag].
#' @export
approximate <- function(x, component = NULL) {
  .soft_deprecate("approximate()", "tag_approximate()")
  tag_approximate(x, component)
}

#' @describeIn defunct Deprecated in messydates 1.2.0. Removed in 1.4.0.
#'   Use `tag_uncertain()`.
#' @export
uncertain <- function(x, component = NULL) {
  .soft_deprecate("uncertain()", "tag_uncertain()")
  tag_uncertain(x, component)
}

#' @describeIn defunct Deprecated in messydates 1.2.0. Removed in 1.4.0.
#'   Use `tag_before()`.
#' @export
on_or_before <- function(x) {
  .soft_deprecate("on_or_before()", "tag_before()")
  tag_before(x)
}

#' @describeIn defunct Deprecated in messydates 1.2.0. Removed in 1.4.0.
#'   Use `tag_after()`.
#' @export
on_or_after <- function(x) {
  .soft_deprecate("on_or_after()", "tag_after()")
  tag_after(x)
}

#' @describeIn defunct Deprecated in messydates 1.2.0. Removed in 1.4.0.
#'   Use `date_is_intersecting()`, which is element-wise.
#' @export
is_intersecting <- function(x, y) {
  .soft_deprecate("is_intersecting()", "date_is_intersecting()")
  date_is_intersecting(x, y)
}

#' @describeIn defunct Deprecated in messydates 1.2.0. Removed in 1.4.0.
#'   Use `date_is_subset()`, which is element-wise.
#' @export
is_subset <- function(x, y) {
  .soft_deprecate("is_subset()", "date_is_subset()")
  date_is_subset(x, y)
}

#' @describeIn defunct Deprecated in messydates 1.2.0. Removed in 1.4.0.
#'   Use `compatible_dates()`.
#'   The argument `by` is not in `compatible_dates()`.
#'   To make a sequence of times, use `seq()`.
#' @param approx_range See [convert_expand].
#' @param by Granularity of enumeration, "day" by default.
#' @export
expand <- function(x, approx_range = 0, by = "day") {
  .soft_deprecate("expand()", "compatible_dates()")
  .compatible(x, approx_range, by)
}

#' @describeIn defunct Deprecated in messydates 1.2.0. Removed in 1.4.0.
#'   Use `as_messydate()` on a list of dates,
#'   or `as_messydate(compatible_dates(x))` for an `mdate`.
#' @param collapse Should a range that covers a whole month or year
#'   become that month or year? `TRUE` by default.
#' @export
contract <- function(x, collapse = TRUE) {
  .soft_deprecate("contract()", "as_messydate()")
  .contract(x, collapse = collapse)
}

#' @describeIn defunct Deprecated in messydates 1.2.0. Removed in 1.4.0.
#'   Use `parse_problems()`.
#' @export
md_problems <- function(x) {
  .soft_deprecate("md_problems()", "parse_problems()")
  parse_problems(x)
}
