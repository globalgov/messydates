#' Contract lists of dates into messy dates
#'
#' This function operates as the opposite of `expand()`.
#' It contracts a list of dates into the abbreviated annotation
#' of messy dates.
#' @param x A list of dates
#' @return A `messydt` vector
#' @importFrom tibble tibble
#' @importFrom lubridate NA_Date_
#' @importFrom stringr str_replace_all
#' @importFrom dplyr lead
#' @examples
#' d <- as_messydate(c("2001-01-01", "2001-01", "2001", "2001-01-01..2001-02-02",
#'        "{2001-01-01,2001-02-02}", "{2001-01,2001-02-02}"))
#' e <- expand(d)
#' tibble::tibble(d,contract(e))
#' @export
contract <- function(x = list()){

  x <- compact_ranges(x)
  x <- collapse_sets(x)
  x <- collapse_ranges(x)

  new_messydate(x)
}

is_sequence <- function(x){
  l <- as.Date(x) + 1
  l <- c(lubridate::NA_Date_,l[-length(l)])
  l <- x==l
  l[is.na(l)] <- FALSE
  l
}

compact_ranges <- function(x){
  sapply(x, function(d){
    if(length(d)>1){
      sequ <- is_sequence(d)
      if(any(sequ)){
        starts <- d[which(sequ==FALSE)]
        ends <- d[dplyr::lead(sequ)==FALSE | is.na(dplyr::lead(sequ))]
        if(any(starts==ends)) ends[starts==ends] <- NA
        d <- paste(starts, ends, sep = "..")
        d <- stringr::str_replace_all(d, "\\.\\.NA", "")
      }
    }
    d
  })
}

collapse_sets <- function(x){
  sapply(x, function(l){
    if(length(l)>1){
      l <- paste(l, collapse = ",")
      l <- paste0("{",l,"}")
    }
    l
  })
}

collapse_ranges <- function(x){
  x <- stringr::str_replace_all(x, "([:digit:]{4})-01-01\\.\\.([:digit:]{4})-12-31", "\\1")
  x <- stringr::str_replace_all(x, "([:digit:]{4}-[:digit:]{2})-01\\.\\.([:digit:]{4}-[:digit:]{2})-28", "\\1")
  x <- stringr::str_replace_all(x, "([:digit:]{4}-[:digit:]{2})-01\\.\\.([:digit:]{4}-[:digit:]{2})-29", "\\1")
  x <- stringr::str_replace_all(x, "([:digit:]{4}-[:digit:]{2})-01\\.\\.([:digit:]{4}-[:digit:]{2})-30", "\\1")
  x <- stringr::str_replace_all(x, "([:digit:]{4}-[:digit:]{2})-01\\.\\.([:digit:]{4}-[:digit:]{2})-31", "\\1")
}
