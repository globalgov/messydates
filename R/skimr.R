#' Adds mdate class `{skimr}` template
#'
#' @description
#' Defines a `{skimr}` template for mdate objects.
#' The helper functions take a `{mdate}` vector as an input and
#' return a single value to be displayed by the `{skimr}` report.
#' The vector is first resolved and yields a single
#' statistic per observation before being summarized again by a function
#' (mean, max, min, etc.) at the vector level.
#' @param column A mdate object.
#' @importFrom skimr get_skimmers
#' @return A `{skimr}` template for mdate objects.
#' @examples
#' d <- tibble::tibble(event = c("Event1", "Event2", "Event3", "Event 4"),
#'                      messydates = as_messydate(c("2001",
#'                                                  "2001-01-01..2003-12-30",
#'                                                  "{2001, 2002, 2003}",
#'                                                  "33 BC")))
#' skimr::skim(d)
#' @export
get_skimmers.mdate <- function(column) {
  skimr::sfl(
    skim_type = "mdate",
    n_empty = skimr::n_empty,
    n_unique = skimr::n_unique,
    max = maxmax,
    min = minmin
    # mean = meanmean
    # median = medianmedian,
    # mode = meanmode,
    # uncertainty = uncertainty
  )
}
