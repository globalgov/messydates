#' Set operations for messy dates
#' @description
#'   `%intersect%` and `%union%` combine two messy dates element by element.
#'   Each element is taken as the set of days that it can be
#'   (see `compatible_dates()`), and the result is an `mdate` in the shortest
#'   notation for the days in the intersection or the union.
#'   For example, the intersection of `"2012-01-01..2012-01-05"` and
#'   `"2012-01-03..2012-01-08"` is `"2012-01-03..2012-01-05"`.
#'
#'   - The two vectors are recycled to a common length, as for the
#'     `date_is_*()` predicates.
#'   - An intersection with no days gives the empty set, `{}`.
#'   - A missing value on either side gives `NA`.
#'   - If either element is uncertain (`?`) or approximate (`~`), the result
#'     carries the same mark on the whole date, because the result is no
#'     more certain than its inputs.
#'   - Days that are not consecutive give a set, `{}`. A `[]` set
#'     ("one member of") gives a `{}` set, because the result lists the days
#'     that remain, not one choice among them.
#' @details
#'   Both kinds of set expand to the same members. For a `{}` set the result
#'   lists the dates that are shared with (or covered by) the other operand.
#'   For a `[]` set it lists the *candidates* that remain possible, so an
#'   empty intersection rules the other operand out rather than showing no
#'   overlap.
#'   The operators work on days, so a time of day is dropped.
#' @name operate_set
#' @param e1,e2 `mdate` objects, or objects that `as_messydate()` can read.
#' @return An `mdate` vector with the common length of `e1` and `e2`.
NULL

#' @rdname operate_set
#' @export
`%intersect%` <- function(e1, e2) UseMethod("%intersect%")

#' @describeIn operate_set Find intersection of sets of messy dates
#' @examples
#' as_messydate("2012-01-01..2012-01-20") %intersect% as_messydate("2012-01")
#' @export
`%intersect%.mdate` <- function(e1, e2) .set_operation(e1, e2, intersect)

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
`%union%.mdate` <- function(e1, e2) .set_operation(e1, e2, union)

evalqOnLoad({
  registerS3method("%union%", "Date", `%union%.mdate`)
  registerS3method("%union%", "POSIXt", `%union%.mdate`)
})

# Applies a set operation on days to each pair of elements, and writes each
# result in its shortest notation. A ? or ~ on either side is carried into
# the result on the whole date.
.set_operation <- function(e1, e2, op) {
  if (!is_messydate(e1)) e1 <- suppressMessages(as_messydate(e1))
  if (!is_messydate(e2)) e2 <- suppressMessages(as_messydate(e2))
  common <- vctrs::vec_recycle_common(e1, e2)
  e1 <- common[[1]]
  e2 <- common[[2]]
  out <- rep(NA_character_, length(e1))
  ok <- !is.na(e1) & !is.na(e2)
  if (!any(ok)) return(new_messydate(out))
  days <- mapply(function(a, b) .sort_days(op(strip_times(a), strip_times(b))),
                 compatible_dates(e1[ok]), compatible_dates(e2[ok]),
                 SIMPLIFY = FALSE, USE.NAMES = FALSE)
  res <- as_messydate(days)
  unc <- (date_is_uncertain(e1[ok]) | date_is_uncertain(e2[ok])) %in% TRUE
  apx <- (date_is_approximate(e1[ok]) | date_is_approximate(e2[ok])) %in% TRUE
  full <- !is_empty_notation(res)
  if (any(apx & full)) res[apx & full] <- tag_approximate(res[apx & full], "date")
  if (any(unc & full)) res[unc & full] <- tag_uncertain(res[unc & full], "date")
  out[ok] <- as.character(res)
  new_messydate(out)
}

# Sorts days in time order, so that dates before the common era sort by
# time and not as text.
.sort_days <- function(d) {
  if (length(d) < 2) return(d)
  key <- as.numeric(as.Date(new_messydate(strip_times(d)), FUN = identity))
  d[order(key, d)]
}
