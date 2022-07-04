#' Data report for datasets with 'mdate' variables
#'
#' Create a properly formatted data report for datasets which contain 'mdate'
#' class objects, alongside other object classes.
#' @param data A `{tibble}` or a `{data.frame}`.
#' @return A data report of class 'mreport'.
#' @importFrom dplyr %>%
#' @importFrom scales percent_format
#' @details 'mreport' displays the variable's name,
#' the variable type, the number of missing observations for variable,
#' the percentage of missing observations in variable,
#' the minimal value in variable, and the maximal value in variable.
#' @examples
#' mreport(battles)
#' @export
mreport <- function(data) UseMethod("mreport")

#' @export
mreport.default <- function(data) {
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
  minv <- sapply(data, function(x) ifelse(class(x) == "mdate",
                                          as.character(as.Date(x, max)), min(x)))
  minv <- ifelse(nchar(minv) > 11, "", minv)
  maxv <- sapply(data, function(x) ifelse(class(x) == "mdate",
                                          as.character(as.Date(x, max)), max(x)))
  maxv <- ifelse(nchar(maxv) > 11, "", maxv)
  result <- list(Rows          = rows,
                 Columns       = cols,
                 Variables     = varnames,
                 Types         = datatype,
                 Count         = counts,
                 Missing       = mvalues,
                 MissingPer    = mvaluesper,
                 Minv = minv,
                 Maxv = maxv)
  class(result) <- "mreport"
  return(result)
}

#' @export
print.mreport <- function(x, ...) {
  print_mreport(x)
}

print_mreport <- function(x) {
  columns <- c("  Column Name  ", "  Data Type  ", "  Missing  ",
               "  Missing (%)  ", "  Min Value  ", "  Max Value ")
  len_col <- as.vector(sapply(columns, nchar))
  x$Types <- lapply(x$Types, paste, collapse = ", ")
  lengths <- list(x$Variables, x$Types, x$Missing, x$MissingPer, x$Minv, x$Maxv)
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
    cat(format(columns[i], width = clengths[i], justify = "centre"),
        "|", sep = "")
  }
  cat("\n", rep("-", dash), sep = "")
  cat("\n")
  for (i in seq_len(x$Columns)) {
    cat("|", format(x$Variables[i], width = clengths[1], justify = "centre"), "|",
      format(x$Types[i], width = clengths[2], justify = "centre"), "|",
      format(as.character(x$Missing[i]), width = clengths[3], justify = "centre"), "|",
      format(as.character(x$MissingPer[i]), width = clengths[4], justify = "centre"), "|",
      format(as.character(x$Min[i]), width = clengths[5], justify = "centre"), "|",
      format(as.character(x$Max[i]), width = clengths[6], justify = "centre"),
      "|\n", sep = "")
  }
  cat(rep("-", dash), sep = "")
  cat("\n\n")
}
