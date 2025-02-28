#' @export
`[.mdate` <- function(x, ..., drop = TRUE) {
  as_messydate(NextMethod("[", unclass(x)))
}

#' @export
`[<-.mdate` <- function(x, i, ..., value) {
  value <- as_messydate(value)
  validate_messydate(value)
  as_messydate(NextMethod("[<-", unclass(x)))
}

#' @export
`[[.mdate` <- function(x, ...) {
  as_messydate(NextMethod("[[", unclass(x)))
}

#' @export
`[[<-.mdate` <- function(x, i, ..., value) {
  value <- as_messydate(value)
  validate_messydate(value)
  as_messydate(NextMethod("[[<-", unclass(x)))
}

#' @export
c.mdate <- function(...) {
  if(length(list(...)) == 1){
    unclass(list(...)[[1]])
  } else {
    vecs <- lapply(list(...), function(e) unclass(as_messydate(e)))
    x <- as_messydate(unlist(vecs))
    validate_messydate(x)
  }
}

#' @export
rep.mdate <- function(x, ...) {
  as_messydate(NextMethod("rep", unclass(x)))
}

# Printing ####

#' @importFrom utils str
#' @export
print.mdate <- function(x, ...) {
  str(x)
}
#' @importFrom utils str
#' @export
print.mdates_duration <- function(x, ...) {
  str(x)
}


