#' Summary statistic functions
#'
#' `{messydates}` provides extensions to a variety of functions
#' with R's `{stats}` package to simplify the creation of
#' summary reports of data with `{skimr}`.
#' All functions take a `messydt` vector as an input and return a single
#' statistic.
#' Note that individual dates are first expanded and resolved
#' before a vector level statistic is computed
#' (e.g. `maxmax()` corresponds to the maximum date of the maximum
#' of the vector resulting from an expanded `{messydt}`)
#' @seealso [get_skimmers()] for customizing the functions called by [skim()].
#' @param date A `messydt` vector
#' @return A statistic describing the `messydt` vector.
#' @name stats
NULL

#' @describeIn stats Calculate the farthest date forward of a `messydt` vector
#' @export
maxmax <- function(date) {
  max(as.Date(date, max), na.rm = TRUE)
}

#' @describeIn stats Calculate the farthest date in the past of a `messydt`
#' vector
#' @export
minmin <- function(date) {
  min(as.Date(date, min), na.rm = TRUE)
}

#' #' @describeIn stats Calculate the mean date of a `messydt` vector
#' #' @export
# meanmean <- function(date) {
#   mean(as.Date(date, mean), na.rm = TRUE)
# }

#' #' @describeIn stats Calculate the median date of a `messydt` vector
#' #' @export
#' medianmedian <- function(date) {
#'   median(as.Date(date, median), na.rm = TRUE)
#' }
#'
#' #' @describeIn stats Calculate the mean of the most frequent date of a
#' #' `messydt` vector
#' #' @export
#' meanmode <- function(date) {
#'   mean(as.Date(date, modal), na.rm = TRUE)
#' }

# # Computing a simple uncertainty measure for vectors.
# # Expressed in number of days.
# uncertainty <- function(date) {
#   sum(as.integer(messyvar(date))) / length(date)
# }
# # Messyvariance computes the range of the possible uncertain dates.
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
