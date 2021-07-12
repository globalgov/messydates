#' Make Messydates from Multiple Variables
#'
#' Transforms multiple date inputs contained in different columns into one.
#' @param ... One (ymd) or three (yyyy, mm, dd) variables
#' @importFrom purrr map
#' @return A character vector containing the
#' @examples
#' make_messydate("2010", "10", "10")
#' @export
make_messydate <- function(...) {
  dots <- list(...)
  if (length(dots) == 1) {
    dots <- do.call(as.character, dots)
    dates <- unlist(dots)
  } else if (length(dots) == 3) {
    dots <- purrr::map(dots, as.character)
    dates <- unlist(purrr::pmap_chr(dots, paste, sep = "-"))
  } else stop("Either you need to pass make_messydate() one variable (i.e. 'yyyy-mm-dd' three (yyyy, mm, dd).")
  dates
}
