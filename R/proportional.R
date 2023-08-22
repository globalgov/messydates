#' Proportion of messy dates meeting logical test
#'
#' These functions provide various proportional tests for messy date objects.
#' @name proportional
#' @param e1,e2 `mdate` or other class objects
#' @return The proportion that the comparison is true.
#' @return A logical vector the same length as the `mdate` passed.
NULL

#' @rdname proportional
#' @export
`%leq%` <- function(e1, e2) UseMethod("%leq%")

#' @describeIn proportional Tests proportion of dates in the first vector
#'   that precede the minimum in the second vector.
#' @examples
#'   as_messydate("2012-06") < as.Date("2012-06-02")
#'   as_messydate("2012-06") %leq% "2012-06-02"
#' @export
`%leq%.mdate` <- function(e1, e2) {
  if(length(e1)!=length(e2))
    stop("Can only compare vectors of equal length.")
  # Need to fix this for element wise on vectors...
  suppressMessages(purrr::map2_dbl(expand(e1), expand(e2),
                                   ~ mean(.x < min(.y))))
}

evalqOnLoad({
  registerS3method("%leq%", "Date", `%leq%.mdate`)
  registerS3method("%leq%", "POSIXt", `%leq%.mdate`)
})

#' @rdname proportional
#' @export
`%geq%` <- function(e1, e2) UseMethod("%geq%")

#' @describeIn proportional Tests proportion of dates in the first vector
#'   that follow the maximum in the second vector.
#' @export
#' @examples
#'   as_messydate("2012-06") > as.Date("2012-06-02")
#'   as_messydate("2012-06") %geq% "2012-06-02"
`%geq%.mdate` <- function(e1, e2) {
  if(length(e1)!=length(e2))
    stop("Can only compare vectors of equal length.")
  # Need to fix this for element wise on vectors...
  suppressMessages(purrr::map2_dbl(expand(e1), expand(e2),
                                   ~ mean(.x > max(.y))))
}

evalqOnLoad({
  registerS3method("%geq%", "Date", `%geq%.mdate`)
  registerS3method("%geq%", "POSIXt", `%geq%.mdate`)
})

#' @rdname proportional
#' @export
`%geqq%` <- function(e1, e2) UseMethod("%geqq%")

#' @describeIn proportional Tests proportion of dates in the first vector
#'   that follow or are equal to the maximum in the second vector.
#' @export
#' @examples
#'   as_messydate("2012-06") >= as.Date("2012-06-02")
#'   as_messydate("2012-06") %geqq% "2012-06-02"
`%geqq%.mdate` <- function(e1, e2) {
  if(length(e1)!=length(e2))
    stop("Can only compare vectors of equal length.")
  # Need to fix this for element wise on vectors...
  suppressMessages(purrr::map2_dbl(expand(e1), expand(e2),
                                   ~ mean(.x >= max(.y))))
}

evalqOnLoad({
  registerS3method("%geqq%", "Date", `%geqq%.mdate`)
  registerS3method("%geqq%", "POSIXt", `%geqq%.mdate`)
})

#' @rdname proportional
#' @export
`%leqq%` <- function(e1, e2) UseMethod("%leqq%")

#' @describeIn proportional Tests proportion of dates in the first vector
#'   that precede or are equal to the minimum in the second vector.
#' @export
#' @examples
#'   as_messydate("2012-06") <= as.Date("2012-06-02")
#'   as_messydate("2012-06") %leqq% "2012-06-02"
`%leqq%.mdate` <- function(e1, e2) {
  if(length(e1)!=length(e2))
    stop("Can only compare vectors of equal length.")
  # Need to fix this for element wise on vectors...
  suppressMessages(purrr::map2_dbl(expand(e1), expand(e2),
                                   ~ mean(.x <= min(.y))))
}

evalqOnLoad({
  registerS3method("%leqq%", "Date", `%leqq%.mdate`)
  registerS3method("%leqq%", "POSIXt", `%leqq%.mdate`)
})

#' @rdname proportional
#' @export
`%><%` <- function(e1, e2) UseMethod("%><%")

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
  suppressMessages(purrr::map2_dbl(e1, e2,
                  ~ length(md_intersect(.x, .y))/
                    (length(unlist(expand(.x)))+1)))
}

evalqOnLoad({
  registerS3method("%><%", "Date", `%><%.mdate`)
  registerS3method("%><%", "POSIXt", `%><%.mdate`)
})

#' @rdname proportional
#' @export
`%>=<%` <- function(e1, e2) UseMethod("%>=<%")

#' @describeIn proportional Tests proportion of dates in the first vector that
#'   are between the minimum and maximum dates in the second vector, inclusive.
#' @export
#' @examples
#'   as_messydate("2012-06") %>=<% as_messydate("2012-06-15..2012-07-15")
`%>=<%.mdate` <- function(e1, e2) {
  if(length(e1)!=length(e2))
    stop("Can only compare vectors of equal length.")
  # Need to fix this for element wise on vectors...
  suppressMessages(purrr::map2_dbl(e1, e2, ~ length(md_intersect(.x, .y))/
                                     length(unlist(expand(.x)))))
}

evalqOnLoad({
  registerS3method("%>=<%", "Date", `%>=<%.mdate`)
  registerS3method("%>=<%", "POSIXt", `%>=<%.mdate`)
})
