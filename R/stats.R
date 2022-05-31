#' Summary data reports for messy dates
#'
#' Provides variable level summaries of `mdate` objects to
#' simplify the creation of data reports with `{skimr}`.
#' @param date An `mdate` vector
#' @return A statistic describing the `mdate` vector.
#' @details Note that individual dates are first expanded and resolved
#' before a vector level statistic is computed
#' (e.g. `maxmax()` corresponds to the maximum date of the maximum
#' of the vector resulting from an expanded `{mdate}`).
#' @examples
#' \dontrun{
#' d <- tibble::tibble(event = c("Event1", "Event2", "Event3", "Event 4"),
#'                     messydates = as_messydate(c("2001",
#'                                                 "2001-01-01..2003-12-30",
#'                                                 "{2001, 2002, 2003}",
#'                                                 "33 BC")))
#' skimr::skim(d)
#' }
#' @name stats
NULL

#' @describeIn stats Calculate the farthest date forward of a `mdate` vector
#' @export
maxmax <- function(date) {
  max(as.Date(date, max), na.rm = TRUE)
}

#' @describeIn stats Calculate the farthest date in the past of a `mdate`
#' vector
#' @export
minmin <- function(date) {
  min(as.Date(date, min), na.rm = TRUE)
}

# #' @describeIn stats Calculate the mean date of a `mdate` vector
# #' @export
# meanmean <- function(date) {
#   mean(as.Date(date, mean), na.rm = TRUE)
# }

# #' @describeIn stats Calculate the median date of a `mdate` vector
# #' @export
# medianmedian <- function(date) {
#   median(as.Date(date, median), na.rm = TRUE)
# }
#
# #' @describeIn stats Calculate the mean of the most frequent date of a
# #' `mdate` vector
# #' @export
# meanmode <- function(date) {
#   mean(as.Date(date, modal), na.rm = TRUE)
# }

# # Computing a simple uncertainty measure for vectors.
# # Expressed in number of days.
# uncertainty <- function(date) {
#   sum(as.integer(messyvar(date))) / length(date)
# }
# # Messy variance computes the range of the possible uncertain dates.
# messyvar <- function(date) {
#   # Resolve
#   resolved <- data.frame(as.Date(date, max), as.Date(date, min))
#   # Compute uncertainty
#   vec <- NULL
#   for (i in 1:nrow(resolved)) {
#     vec[i] <- resolved[i, 1] - resolved[i, 2]
#   }
#   vec
# }

#' @importFrom skimr get_skimmers
#' @export
get_skimmers.mdate <- function(date) {
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
