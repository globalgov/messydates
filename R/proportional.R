#' Proportion of messy dates meeting logical test
#'
#' These functions provide various proportional tests for messy date objects.
#' @name proportional
#' @param x,y `mdate` or other class objects
#' @return A logical vector the same length as the `mdate` passed.
NULL

#' @export
`%<%` <- function(e1, e2) UseMethod("%<%")

#' @usage e1 %<% e2
#' @describeIn proportional Tests proportion of dates in the first vector
#'   that precede the minimum in the second vector.
#' @export
#' @examples
#'   as_messydate("2012-06") < as.Date("2012-06-02")
#'   as_messydate("2012-06") %<% "2012-06-02"
`%<%.mdate` <- function(e1, e2) {
  if(length(e1)!=length(e2))
    stop("Can only compare vectors of equal length.")
  # Need to fix this for element wise on vectors...
  purrr::map2_dbl(expand(e1), suppressMessages(expand(e2)), ~ mean(.x < min(.y)))
}

evalqOnLoad({
  registerS3method("%<%", "Date", `%<%.mdate`)
  registerS3method("%<%", "POSIXt", `%<%.mdate`)
})

#' @export
`%>%` <- function(e1, e2) UseMethod("%>%")

#' @usage e1 %>% e2
#' @describeIn proportional Tests proportion of dates in the first vector
#'   that follow the maximum in the second vector.
#'   Note that this conflicts with `{magrittr}`'s pipe,
#'   and so the base R pipe, `|>` is recommended instead.
#' @export
#' @examples
#'   as_messydate("2012-06") > as.Date("2012-06-02")
#'   as_messydate("2012-06") %>% "2012-06-02"
`%>%.mdate` <- function(e1, e2) {
  if(length(e1)!=length(e2))
    stop("Can only compare vectors of equal length.")
  # Need to fix this for element wise on vectors...
  purrr::map2_dbl(expand(e1), suppressMessages(expand(e2)), ~ mean(.x > max(.y)))
}

evalqOnLoad({
  registerS3method("%>%", "Date", `%>%.mdate`)
  registerS3method("%>%", "POSIXt", `%>%.mdate`)
})

#' @export
`%>=%` <- function(e1, e2) UseMethod("%>=%")

#' @usage e1 %>=% e2
#' @describeIn proportional Tests proportion of dates in the first vector
#'   that follow or are equal to the maximum in the second vector.
#' @export
#' @examples
#'   as_messydate("2012-06") >= as.Date("2012-06-02")
#'   as_messydate("2012-06") %>=% "2012-06-02"
`%>=%.mdate` <- function(e1, e2) {
  if(length(e1)!=length(e2))
    stop("Can only compare vectors of equal length.")
  # Need to fix this for element wise on vectors...
  purrr::map2_dbl(expand(e1), suppressMessages(expand(e2)), ~ mean(.x >= max(.y)))
}

evalqOnLoad({
  registerS3method("%>=%", "Date", `%>=%.mdate`)
  registerS3method("%>=%", "POSIXt", `%>=%.mdate`)
})

#' @export
`%<=%` <- function(e1, e2) UseMethod("%<=%")

#' @usage e1 %<=% e2
#' @describeIn proportional Tests proportion of dates in the first vector
#'   that precede or are equal to the minimum in the second vector.
#' @export
#' @examples
#'   as_messydate("2012-06") <= as.Date("2012-06-02")
#'   as_messydate("2012-06") %<=% "2012-06-02"
`%<=%.mdate` <- function(e1, e2) {
  if(length(e1)!=length(e2))
    stop("Can only compare vectors of equal length.")
  # Need to fix this for element wise on vectors...
  purrr::map2_dbl(expand(e1), suppressMessages(expand(e2)), ~ mean(.x <= min(.y)))
}

evalqOnLoad({
  registerS3method("%<=%", "Date", `%<=%.mdate`)
  registerS3method("%<=%", "POSIXt", `%<=%.mdate`)
})

#' @export
`%><%` <- function(e1, e2) UseMethod("%><%")

#' @usage e1 %><% e2
#' @describeIn proportional Tests proportion of dates in the first vector
#'   that are between the minimum and maximum dates in the second vector.
#' @export
#' @examples
#'   as_messydate("2012-06") %><% as_messydate("2012-06-15..2012-07-15")
`%><%.mdate` <- function(e1, e2) {
  if(length(e1)!=length(e2))
    stop("Can only compare vectors of equal length.")
  # Need to fix this for element wise on vectors...
  # Need to create fast way to trim ranges or just get dates within the range
  purrr::map2_dbl(expand(e1), suppressMessages(expand(e2)),
                  ~ (length(md_intersect(.x, .y)))/length(.x))
}

evalqOnLoad({
  registerS3method("%><%", "Date", `%><%.mdate`)
  registerS3method("%><%", "POSIXt", `%><%.mdate`)
})

#' @export
`%>=<%` <- function(e1, e2) UseMethod("%>=<%")

#' @usage e1 %>=<% e2
#' @describeIn proportional Tests proportion of dates in the first vector
#'   that are between the minimum and maximum dates in the second vector, inclusive.
#' @export
#' @examples
#'   as_messydate("2012-06") %>=<% as_messydate("2012-06-15..2012-07-15")
`%>=<%.mdate` <- function(e1, e2) {
  if(length(e1)!=length(e2))
    stop("Can only compare vectors of equal length.")
  # Need to fix this for element wise on vectors...
  purrr::map2_dbl(expand(e1), suppressMessages(expand(e2)),
                  ~ length(md_intersect(.x, .y))/length(.x))
}

evalqOnLoad({
  registerS3method("%>=<%", "Date", `%>=<%.mdate`)
  registerS3method("%>=<%", "POSIXt", `%>=<%.mdate`)
})

