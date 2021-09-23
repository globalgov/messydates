#' Resorting and filtering dates
#'
#' Resorting and filtering dates
#' @param data a dataframe
#' @param vars a character vector identifying columns
#' in the dataframe to sequence
#' @param unity a string identifying how multiple
#' entries may be glued together.
#' By default, tidyr::unite() glues using the underscore "_".
#' @return a dataframe/columns
#' @import lubridate
#' @importFrom stats na.omit
#' @examples
#' \dontrun{
#' data <- data.frame(Sign = c("2000-01-01", "2001-01-01",
#' "2001-01-01_2000-01-01", "2000-01-01", NA),
#' Force = c("2001-01-01", "2000-01-01",
#' "2001-01-01", NA, "2001-01-01"))
#' resequence(data, c("Sign", "Force"))
#' }
#' @export
resequence <- function(data, vars, unity = "_") {

  len <- length(vars)

  out <- apply(data[, vars], 1, function(x) {
    dates <- sort(unlist(strsplit(unique(na.omit(x)), unity)))

    if (length(dates) < len) {
      dates <- interleave(dates, which(is.na(x)))
    }

    if (length(dates) > len) {
      if (sum((!grepl("-01-01", dates)) * 1) >= len) dates <- dates[!grepl("-01-01", dates)]
      if (sum((!grepl("9999", dates)) * 1) >= len) dates <- dates[!grepl("9999", dates)]

      dmax <- max(lubridate::as.duration(interval(dates[1 : (length(dates)-1)],
                                                  dates[2 : (length(dates))])))
      dmax <- which(lubridate::as.duration(interval(dates[1 : (length(dates)-1)],
                                                    dates[2 : (length(dates))])) == as.duration(dmax))
      dates <- dates[c(1, dmax + 1)]
    }

    dates
  })

  t(out)

}

#' Helper function for interleaving two vectors by position
#'
#' Insert elements in different positions for vectors
#' @param vect Main vector
#' @param pos Positions to be inserted
#' @param elems Elements to be inserted at those positions.
#' By default, these are NAs (missing values).
#' @return A vector the length of the sum of \code{vect}
#' and \code{pos}.
interleave <- function(vect, pos, elems = NA) {

  j <- 0
  for (k in seq_len(length(pos))) {
    if (pos[k] == 1)
      vect <- c(elems[j + 1], vect)
    else if (pos[k] == length(vect) + 1)
      vect <- c(vect, elems[j + 1])
    else
      vect <- c(vect[1:(pos[k] - 1)],
                elems[j + 1],
                vect[(pos[k]):length(vect)])
    j <- j + 1
  }
  return(vect)
}
