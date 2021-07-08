#' @export
min.messydt <- function(..., na.rm = TRUE){
  x <- list(...)
  y <- expand(x[[1]])
  y <- sapply(y, function(x) as.character(min(x, na.rm = na.rm)))
  y
}

#' @export
max.messydt <- function(..., na.rm = TRUE){
  x <- list(...)
  y <- expand(x[[1]])
  y <- sapply(y, function(x) as.character(max(x, na.rm = na.rm)))
  y
}

#' @export
max.messydt <- function(..., na.rm = TRUE){
  x <- list(...)
  y <- expand(x[[1]])
  y <- sapply(y, function(x) as.character(max(x, na.rm = na.rm)))
  y
}

#' @export
median.messydt <- function(..., na.rm = TRUE){
  x <- list(...)
  y <- expand(x[[1]])
  y <- sapply(y, function(x) as.character(median(x, na.rm = na.rm)))
  y
}

#' @export
mean.messydt <- function(..., na.rm = TRUE){
  x <- list(...)
  y <- expand(x[[1]])
  y <- sapply(y, function(x){
    if(length(x)>1) x <- as.character(mean(x, na.rm = na.rm))
    x
  }) 
  y
}

#' @export
modal <- function(..., na.rm = FALSE) UseMethod("modal")

#' @export
modal.messydt <- function(..., na.rm = TRUE){
  x <- list(...)
  y <- expand(x[[1]])
  getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  y <- sapply(y, function(x){
    if(length(x)>1) x <- as.character(getmode(x))
    x
  }) 
  y
}

