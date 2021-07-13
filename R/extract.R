#' Extracting components from messy dates
#' @name extract
NULL
#> NULL

#' @rdname extract
#' @examples
#' year(as_messydate(c("2012-02-03","2012","2012-02")))
#' @export
year <- function(x){
  x <- sapply(x, function(x){
    stringr::str_split(x, "-")[[1]][1]
  })
  as.numeric(x)
}

#' @rdname extract
#' @examples
#' month(as_messydate(c("2012-02-03","2012","2012-02")))
#' @export
month <- function(x){
  x <- sapply(x, function(x){
    stringr::str_split(x, "-")[[1]][2]
  })
  as.numeric(x)
}

#' @rdname extract
#' @examples
#' day(as_messydate(c("2012-02-03","2012","2012-02")))
#' @export
day <- function(x){
  x <- sapply(x, function(x){
    stringr::str_split(x, "-")[[1]][3]
  })
  as.numeric(x)
}
