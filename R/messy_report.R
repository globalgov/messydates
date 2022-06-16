#' Messy data report
#'
#' @param data A \code{tibble} or a \code{data.frame}.
#' @return \code{messy_report()()} returns a data report
#' of class \code{"messy_report"}.
#' @import ggplot2
#' @importFrom dplyr %>%
#' @importFrom scales percent_format
#' @examples
#' messy_report(battles)
#' @export
messy_report <- function(data) UseMethod("messy_report")

#' @export
messy_report.default <- function(data) {
  if (!is.data.frame(data)) {
    stop("Data must be a `data.frame` or `tibble`.")
  }
  rows     <- nrow(data)
  cols     <- ncol(data)
  varnames <- names(data)
  datatype <- sapply(data, class)
  counts   <- sapply(data, length)
  mvalues    <- sapply(data, function(z) sum(is.na(z)))
  mvaluesper <- round((mvalues / counts) * 100, 2)
  result <- list(Rows          = rows,
                 Columns       = cols,
                 Variables     = varnames,
                 Types         = datatype,
                 Count         = counts,
                 Missing       = mvalues,
                 MissingPer    = mvaluesper)
  class(result) <- "messy_report"
  return(result)
}

#' @export
print.messy_report <- function(x, ...) {
  print_messy_report(x)
}

#' @export
plot.messy_report <- function(x, ...) {
  `% Missing`  <- y <- NULL
  mydat        <- data.frame(x = names(x$MissingPer), y = x$MissingPer)
  mydat$y      <- mydat$y / 100
  mydat$color  <- ifelse(mydat$y >= 0.1, ">= 10%", "< 10%")
  names(mydat) <- c("x", "y", "% Missing")
  ggplot2::ggplot(mydat) +
    ggplot2::geom_col(ggplot2::aes(x = stats::reorder(x, y), y = y, fill = `% Missing`)) +
    ggplot2::scale_y_continuous(labels = scales::percent_format()) +
    ggplot2::xlab("Column") + ggplot2::ylab("Percentage") +
    ggplot2::ggtitle("Missing Values (%)") +
    ggplot2::scale_fill_manual(values = c("green", "red"))
}

print_messy_report <- function(x) {
  columns <- c("  Column Name  ", "  Data Type  ", "  Missing  ", "  Missing (%)  ")
  len_col <- as.vector(sapply(columns, nchar))
  x$Types <- lapply(x$Types, paste, collapse = ", ")
  lengths <- list(x$Variables, x$Types, x$Missing, x$MissingPer)
  n <- length(columns)
  nlist <- list()
  for (i in seq_len(n)) {
    nlist[[i]] <- max(len_col[i], max(sapply(lengths[[i]], nchar)))
  }
  clengths <- unlist(nlist)
  dash <- sum(clengths) + 6
  cat(rep("-", dash), sep = "")
  cat("\n|")
  for (i in seq_len(n)) {
    cat(format(columns[i], width = clengths[i], justify = "centre"), "|", sep = "")
  }
  cat("\n", rep("-", dash), sep = "")
  cat("\n")
  for (i in seq_len(x$Columns)) {
    cat("|", format(x$Variables[i], width = clengths[1], justify = "centre"), "|",
      format(x$Types[i], width = clengths[2], justify = "centre"), "|",
      format(as.character(x$Missing[i]), width = clengths[3], justify = "centre"), "|",
      format(as.character(x$MissingPer[i]), width = clengths[4], justify = "centre"), "|\n", sep = "")
  }
  cat(rep("-", dash), sep = "")
  cat("\n\n")
}
