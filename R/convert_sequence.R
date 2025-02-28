#' Sequence method for messydates
#' @description
#'   This function provides a sequence (`seq()`) method for messydates.
#'   This can be used with ranges or unspecified dates,
#'   and is particularly useful for defining a sequence of dates
#'   before the common era or between eras.
#' @name convert_sequence
#' @param from A messydate or range.
#'   If 'from' is a range and 'to' is not specified,
#'   'from' will be the minimum of the range and 'to' will be maximum.
#' @param to A messydate.
#' @param by Increment of the sequence. By default "days".
#' @param ... Arguments passed to or from methods.
#' @examples
#' seq(mdate("-0001-12-20"), mdate("0001-01-10"))
#' @export
seq.mdate <- function(from, to, by = "days", ...) {

  if(missing(to) & !is_precise(from)){
    to <- max(from)
    from <- min(from)
  }

  # straight forward sequence
  if(!any(is_bce(c(from, to)))){
    seq(as.Date(from), as.Date(to), by = by)
  } else {

    fromp <- as.Date(stringi::stri_replace_first_regex(from, "^-", ""))
    # sequence before common era
    if(is_bce(to)){
      top <- as.Date(stringi::stri_replace_first_regex(to, "^-", ""))
      .neg_seqs(fromp, top, by = by)
    } else {
      # sequence between eras
      zero_padding(c(.neg_seqs(fromp, as.Date("0001-12-31"), by = by),
                     as.character(seq(as.Date("0001-01-01"), as.Date(to), by = by))))
      # zero_padding(c(rev(paste0("-", seq(as.Date("0001-01-01"), fromp, by = by))),
      #   as.character(seq(as.Date("0001-01-01"), as.Date(to), by = by))))
    }
  }
}

.neg_seqs <- function(fromp, top, by = "days"){
  if(year(fromp) == year(top)){
    zero_padding(paste0("-", seq(min(c(fromp, top)),
                                 max(c(fromp,top)), by = by)))
  } else {
    strt <- max(c(fromp, top))
    ends <- min(c(fromp, top))
    strt_yr <- year(strt)
    strt_sq <- seq(as.Date(strt), as.Date(paste0(strt_yr,"-12-31")), by = by)
    ends_yr <- year(ends)
    ends_sq <- seq(as.Date(paste0(ends_yr, "-01-01")), as.Date(ends), by = by)
    if(strt_yr - ends_yr > 1){
      mids_sq <- seq(as.Date(paste0(ends_yr+1, "-01-01")),
                     as.Date(paste0(strt_yr-1,"-12-31")), by = by)
      if(length(unique(year(mids_sq)))>1)
        mids_sq <- mids_sq[order(year(mids_sq), decreasing = TRUE)]
      zero_padding(paste0("-", c(strt_sq, mids_sq, ends_sq)))
    } else zero_padding(paste0("-", c(strt_sq, ends_sq)))
  }
}
