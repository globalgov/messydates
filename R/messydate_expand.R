#' @export
expand <- function(x) UseMethod("expand")

#' @export
expand.messydt <- function(x){
  
  x <- remove_qualifiers(x)
  
  x <- expand_unspecified(x)
  # x <- expand_sets(x)
  x <- expand_ranges(x)
  
  x
}

remove_qualifiers <- function(dates){
  dates <- stringr::str_replace_all(dates, "\\?", "")
  dates <- stringr::str_replace_all(dates, "\\~", "")
  dates <- stringr::str_replace_all(dates, "\\%", "")
  dates
}

expand_unspecified <- function(dates){
  dates <- stringr::str_replace_all(dates, "^([:digit:]{2})XX$", "\\100-01-01..\\199-12-31")
  dates <- stringr::str_replace_all(dates, "^([:digit:]{3})X$", "\\10-01-01..\\19-12-31")
  dates <- stringr::str_replace_all(dates, "^([:digit:]{4})$", "\\1-01-01..\\1-12-31")
  dates <- stringr::str_replace_all(dates, "^([:digit:]{4})-02$", "\\1-02-01..\\1-02-28") # needs to correct for leap years
  dates <- stringr::str_replace_all(dates, "^([:digit:]{4})-09$", "\\1-09-01..\\1-09-30")
  dates <- stringr::str_replace_all(dates, "^([:digit:]{4})-04$", "\\1-04-01..\\1-04-30")
  dates <- stringr::str_replace_all(dates, "^([:digit:]{4})-06$", "\\1-06-01..\\1-06-30")
  dates <- stringr::str_replace_all(dates, "^([:digit:]{4})-11$", "\\1-11-01..\\1-11-30")
  dates <- stringr::str_replace_all(dates, "^([:digit:]{4})-([:digit:]{2})$", "\\1-\\2-01..\\1-\\2-31")
  dates
}

expand_ranges <- function(dates){
  # dates <- stringr::str_replace_all(dates, "\\.\\.", ":")
  dates <- stringr::str_split(dates, "\\.\\.")
  dates <- lapply(dates, function(x){
    if(length(x)==2) x <- seq(as.Date(x[1]), as.Date(x[2]), by = 'days')
    x
  })
  dates
}

