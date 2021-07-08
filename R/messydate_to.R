#' Coercion from messydate
#' @export
as.Date.messydt <- function(x, 
                            resolve = c("min","max","median","mean")){
  
  resolve <- match.arg(resolve)
  
  if(resolve == "min") x <- min(x)
  if(resolve == "max") x <- max(x)
  if(resolve == "mean") x <- mean(x)
  if(resolve == "median") x <- median(x)
  
  as.Date(x)
}

#' @export
as.POSIXct.messydt <- function(x, 
                               resolve = c("min","max","median","mean")){
  
  resolve <- match.arg(resolve)
  
  if(resolve == "min") x <- min(x)
  if(resolve == "max") x <- max(x)
  if(resolve == "mean") x <- mean(x)
  if(resolve == "median") x <- median(x)
  
  as.POSIXct(x)
}

#' @export
as.POSIXlt.messydt <- function(x, 
                               resolve = c("min","max","median","mean")){
  
  resolve <- match.arg(resolve)
  
  if(resolve == "min") x <- min(x)
  if(resolve == "max") x <- max(x)
  if(resolve == "mean") x <- mean(x)
  if(resolve == "median") x <- median(x)
  
  as.POSIXlt(x)
}

