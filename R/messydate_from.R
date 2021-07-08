#' Coercion to messydate
#' @export
as_messydate <- function(x) UseMethod("as_messydate")

#' @export
as_messydate.Date <- function(x){
  x <- as.character(x)
  new_messydate(x)
}

#' @export
as_messydate.POSIXct <- function(x){
  x <- as.character(x)
  new_messydate(x)
}

#' @export
as_messydate.POSIXlt <- function(x){
  x <- as.character(x)
  new_messydate(x)
}

#' @export
as_messydate.character <- function(x){
  
  d <- x
  d <- standardise_date_separators(d)
  d <- standardise_date_order(d)
  d <- standardise_unspecifieds(d)
  d <- standardise_widths(d)
  
  d <- remove_imprecision(d)
  
  new_messydate(d)
}

standardise_date_separators <- function(dates){
  dates <- stringr::str_replace_all(dates, "([:digit:]{4})([:digit:]{2})([:digit:]{2})", "\\1-\\2-\\3")
  dates <- stringr::str_replace_all(dates, "(?<=[:digit:])\\.(?=[:digit:])", "-")
  dates <- stringr::str_replace_all(dates, "\\/", "-")
  dates <- stringr::str_trim(dates, side = "both")
  dates
}

standardise_date_order <- function(dates){
  dates <- stringr::str_replace_all(dates, "([:digit:]{2})-([:digit:]{2})-([:digit:]{4})", "\\3-\\2-\\1")
  dates
}

standardise_widths <- function(dates){
  dates <- stringr::str_replace_all(dates, "-([:digit:])$", "-0\\1")
  dates <- stringr::str_replace_all(dates, "-([:digit:])-", "-0\\1-")
  dates <- stringr::str_replace_all(dates, "^([:digit:])-", "0\\1-")
  dates
}

standardise_unspecifieds <- function(dates){
  dates <- stringr::str_replace_all(dates, "^NA", "XXXX")
  dates <- stringr::str_replace_all(dates, "-NA", "-XX")
  dates <- stringr::str_replace_all(dates, "0000", "XXXX")
  dates <- stringr::str_replace_all(dates, "-00", "-XX")
  dates <- stringr::str_replace_all(dates, "\\?\\?\\?\\?", "XXXX")
  dates <- stringr::str_replace_all(dates, "-\\?\\?", "-XX")
  dates
}

standardise_ranges <- function(dates){
  dates <- stringr::str_replace_all(dates, "_", "..")
  dates <- stringr::str_replace_all(dates, ":", "..")
  dates
}

remove_imprecision <- function(dates){
  dates <- stringr::str_replace_all(dates, "-XX$", "")
  dates <- stringr::str_replace_all(dates, "-XX$", "")
  dates
}

standardise_date_input <- function(dates) {
  
  as_bc_dates <- function(dates) {
    dates <- stringr::str_remove_all(dates, "(bc|BC|Bc|bC)")
    # remove before christ letters
    dates <- paste0("-", dates) # adds a negative sign to date
    dates
  }
  dates <- stringr::str_remove_all(dates, "(ad|AD|Ad|aD)")
  # remove after christ
  dates <- ifelse(stringr::str_detect(dates, "(bc|BC|Bc|bC)"),
                  as_bc_dates(dates), dates)
  # replacing BC for corresponding negative dates
  dates <- stringr::str_trim(dates, side = "both")
  # removes trailing white spaces
}

