#' Extracting components from messy dates
#'
#' These functions allow the extraction of particular date components
#' from messy dates, such as the `year()`, `month()`, and `day()`.
#' `precision()` allows for the identification of the greatest level of
#' precision in (currently) the first element of each date.
#' @param md A messy date object
#' @return `year()`, `month()`, and `day()` extraction return the integer
#' for the requested date component.
#' `precision()` returns the level of greatest precision for each date.
#' @name extract
NULL
#> NULL

#' @rdname extract
#' @examples
#' year(as_messydate(c("2012-02-03","2012","2012-02")))
#' @export
year <- function(md){
  md <- sapply(md, function(x){
    stringr::str_split(x, "-")[[1]][1]
  })
  as.integer(md)
}

#' @rdname extract
#' @examples
#' month(as_messydate(c("2012-02-03","2012","2012-02")))
#' @export
month <- function(md){
  md <- sapply(md, function(x){
    stringr::str_split(x, "-")[[1]][2]
  })
  as.integer(md)
}

#' @rdname extract
#' @examples
#' day(as_messydate(c("2012-02-03","2012","2012-02")))
#' @export
day <- function(md){
  md <- sapply(md, function(x){
    stringr::str_split(x, "-")[[1]][3]
  })
  as.integer(md)
}

#' @rdname extract
#' @importFrom stringr str_remove_all str_count
#' @examples
#' precision(as_messydate(c("2012-02-03","2012","2012-02")))
#' @export
precision <- function(md){
  md <- as.character(md)
  md <- stringr::str_remove_all(md, "\\..*")
  md <- stringr::str_remove_all(md, ",.*")
  count <- stringr::str_count(md, "-")

  out <- ifelse(count==2, "day",
                ifelse(count==1, "month", "year"))
  out
}
