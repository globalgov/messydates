#' Make messy dates from multiple variables
#'
#' Transforms multiple date inputs contained in different columns into one.
#' @param ... One (yyyy-mm-dd) or three (yyyy, mm, dd) variables
#' @param resequence The argument is passed onto `as_messydate()`
#' to allow users to choose the order for ambiguous 6 digit dates
#' (eg. "11-01-12"), and to expand these dates into precise dates
#' (i.e. YYYY-MM-DD format). `FALSE` by default.
#' @importFrom purrr map
#' @return A character vector containing the
#' @examples
#' make_messydate("2010", "10", "10")
#' @export
make_messydate <- function(..., resequence = FALSE) {
  dots <- list(...)
  if (length(dots) == 1) {
    dots <- do.call(as.character, dots)
    dates <- unlist(dots)
  } else if (length(dots) == 3) {
    dots <- purrr::map(dots, as.character)
    dates <- unlist(purrr::pmap_chr(dots, paste, sep = "-"))
    dates <- gsub("NA-NA-NA", "NA", dates)
  } else stop("Pass make_messydate() one variable (yyyy-mm-dd)
              or three variables (yyyy, mm, dd).")
  as_messydate(dates, resequence)
}
