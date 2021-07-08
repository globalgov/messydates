#' A broader date class for messy dates
#' 
#' These functions introduce ISO 8601-2_2019 to R.
#' These recent extensions to standardised date notation
#' create space for unspecified, uncertain, and approximate dates, 
#' as well as succinct representation of date ranges.
#' 
#' Unspecified date components, such as when the day is unknown,
#' can be represented by one or more "X"s in place of the digits.
#' The modifier "*" is recommended to indicate that the entire
#' time scale component value is unspecified, e.g. "X*-03-03",
#' however this is not implemented here.
#' Please be explicit about the digits that are unspecified,
#' e.g. "XXXX-03-03" expresses 3rd March in some unspecified year,
#' whereas "2003-XX-03" expresses the 3rd of some month in 2003.
#' If time components are not given, they are expanded to this.
#' 
#' Approximate date components, modified by "~",
#' represent an estimate whose value is asserted 
#' to be possibly correct.
#' For example, "2003~-03-03"
#' The degree of confidence in approximation 
#' depends on the application.
#' 
#' Uncertain date components, modified by "?",
#' represent a date component whose source is considered
#' to be dubious and therefore not to be relied upon.
#' An additional modifier, "%", is used to indicate
#' a value that is both uncertain and approximate.
#' 
#' These functions also introduce standard notation
#' for ranges of dates.
#' Rather than the typical R notation for ranges,
#' ":", ISO 8601-2_2019(E) recommends "..".
#' This then can be applied between two time scale
#' components to create a standard range between
#' these dates (inclusive), e.g. "2009-01-01..2019-01-01".
#' But it can also be used as an affix,
#' indicating "on or before" if used as a prefix,
#' e.g. "..2019-01-01",
#' or indicating "on or after" if used as a suffix,
#' e.g. "2009-01-01..".
#' 
#' And lastly, notation for sets of dates is also included.
#' Here braces, "{}", are used to mean "all members of the set",
#' while brackets, "[]", are used to mean "one member of the set".
#' @export
new_messydate <- function(x = character()){
  stopifnot(is.character(x))
  structure(x, class = c("messydt"))
}

#' @export
validate_messydate <- function(x){
  values <- unclass(x)
  
  if (any(grepl("[A-WYZa-z]", values))) {
    stop(
      "The only alpha character allowed in messy dates is 'X' for unspecified time components",
      call. = FALSE
    )
  }

    if (!all(grepl("[0-9]", values))) {
    stop(
      "Messy dates require at least one specified date component.",
      call. = FALSE
    )
    }
  
  # can only consist of numbers and some special symbols: []{}..X%?~"
  
  x
}

#' @export
print.messydt <- function(x){
  str(x)
}
