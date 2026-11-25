# Testing proportional methods of messydates

test_that("proportional methods work", {
  expect_equal(class(as_messydate("2012-06-02") %l% as_messydate("2012-06")),
               "numeric")
  expect_equal(class(as_messydate("2012-06") %g%
                       as_messydate("2012-06-10")), "numeric")
  expect_equal(class(as_messydate("2012-06") %le%
                       as_messydate("2012-06-10")), "numeric")
  expect_equal(class(as_messydate("2012-06") %ge%
                       as_messydate("2012-06-10")), "numeric")
  expect_equal(class(as_messydate("2012-06") %><%
                       as_messydate("2012-06-15..2012-07-15")), "numeric")
  expect_equal(class(as_messydate("2012-06") %>=<%
                       as_messydate("2012-06-15..2012-07-15")), "numeric")
  expect_equal(round(as_messydate("2012-06") %l% as_messydate("2012-06-10"), 2), 0.3)
  expect_equal(round(as_messydate("2012-06") %g%
                       as_messydate("2012-06-10"), 2), 0.67)
  expect_equal(round(as_messydate("2012-06") %le%
                       as_messydate("2012-06-10"), 2), 0.33)
  expect_equal(round(as_messydate("2012-06") %ge% as_messydate("2012-06-10"), 2), 0.70)
  expect_equal(round(as_messydate("2012-06") %><%
                       as_messydate("2012-06-15..2012-07-15"), 2), 0.52)
  expect_equal(round(as_messydate("2012-06") %>=<%
                       as_messydate("2012-06-15..2012-07-15"), 2), 0.53)
})

test_that("proportional methods return 1 or 0 for fully separated dates", {
  expect_equal(as_messydate("2012-01-01") %l% as_messydate("2012-06-10"), 1)
  expect_equal(as_messydate("2012-12-31") %g% as_messydate("2012-06-10"), 1)
  expect_equal(as_messydate("2012-12-31") %l% as_messydate("2012-06-10"), 0)
})

test_that("proportional methods dispatch for Date and POSIXt", {
  expect_equal(as.Date("2012-01-01") %l% as_messydate("2012-06-10"), 1)
  expect_equal(as.POSIXct("2012-01-01", tz = "UTC") %l% as_messydate("2012-06-10"), 1)
  expect_equal(as.Date("2012-12-31") %g% as_messydate("2012-06-10"), 1)
  expect_equal(as.POSIXct("2012-12-31", tz = "UTC") %g% as_messydate("2012-06-10"), 1)
  expect_equal(as.Date("2012-06-20") %ge% as_messydate("2012-06-01..2012-06-20"), 1)
  expect_equal(as.Date("2012-06-01") %le% as_messydate("2012-06-01..2012-06-20"), 1)
  # %><% divides by length(compatible_dates(e1)) + 1, so even a single contained day
  # cannot reach a full 1
  expect_equal(as.Date("2012-06-10") %><% as_messydate("2012-06-01..2012-06-20"), 0.5)
  expect_equal(as.Date("2012-06-10") %>=<% as_messydate("2012-06-01..2012-06-20"), 1)
})

test_that("proportional methods error on mismatched vector lengths", {
  expect_error(as_messydate(c("2012-01", "2012-02")) %l% as_messydate("2012-06-10"))
  expect_error(as_messydate(c("2012-01", "2012-02")) %g% as_messydate("2012-06-10"))
  expect_error(as_messydate(c("2012-01", "2012-02")) %le% as_messydate("2012-06-10"))
  expect_error(as_messydate(c("2012-01", "2012-02")) %ge% as_messydate("2012-06-10"))
  expect_error(as_messydate(c("2012-01", "2012-02")) %><% as_messydate("2012-06-10"))
  expect_error(as_messydate(c("2012-01", "2012-02")) %>=<% as_messydate("2012-06-10"))
})

test_that("the battles data holds a current mdate", {
  expect_s3_class(battles$Date, c("mdate", "vctrs_vctr"))
  november <- rep(as_messydate("2001-11"), nrow(battles))
  expect_length(battles$Date %>=<% november, nrow(battles))
})

test_that("proportions from range ends agree with the listed days", {
  x <- as_messydate(c("2012-06", "2012-06-15", "2011-12-20..2012-01-10",
                      "2012-XX-15", "{2012-06-01,2012-06-20}", NA))
  y <- as_messydate(c("2012-06-10..2012-06-20", "2012-06", "2012",
                      "2012-06", "2012-06-05..2012-06-25", "2012-06"))
  expect_equal(x %l% y, c(9/30, 0, 12/22, 5/12, 1/2, NA))
  expect_equal(x %ge% y, c(11/30, 0, 0, 6/12, 0, NA))
  expect_equal(x %>=<% y, c(11/30, 1, 10/22, 1/12, 1/2, NA))
  expect_equal(x %><% y, c(11/31, 1/2, 10/23, 1/13, 1/3, NA))
})

test_that("approx_range weights the days of approximate values", {
  x <- as_messydate("2012-06-~15")
  y <- as_messydate("2012-06-15")
  # 12 to 18 June, with weights 0.25, 0.5, 0.75, 1, 0.75, 0.5 and 0.25
  expect_equal(`%l%`(x, y, approx_range = 3), 1.5 / 4)
  expect_equal(`%le%`(x, y, approx_range = 3), 2.5 / 4)
  expect_equal(`%>=<%`(x, y, approx_range = 3), 1 / 4)
  z <- as_messydate(c("2012-06-~15", "2012-06", NA))
  w <- as_messydate(c("2012-06-16", "2012-06-~15", "2012-06"))
  for (op in c("%l%", "%g%", "%le%", "%ge%", "%><%", "%>=<%"))
    expect_identical(get(op)(z, w, approx_range = 0), get(op)(z, w))
  expect_error(`%l%`(x, y, approx_range = -1), "approx_range")
})

test_that("every proportion operator gives NA for a missing value", {
  x <- as_messydate(c(NA, "2012-06"))
  for (op in c("%l%", "%g%", "%le%", "%ge%", "%><%", "%>=<%"))
    expect_true(is.na(get(op)(x, rev(x))[1]))
})

test_that("every proportion operator gives numeric(0) for inputs of length 0", {
  x <- new_messydate(character(0))
  for (op in c("%l%", "%g%", "%le%", "%ge%", "%><%", "%>=<%"))
    expect_identical(get(op)(x, x), numeric(0))
})

test_that("date_by_intersecting() gives the share of day pairs within a tolerance", {
  x <- as_messydate(c("2001-05-16", "2001-05-13..2001-05-19", "2001-05-13..2001-05-19", NA, "{}"))
  y <- as_messydate(c("2001-05-17", "2001-05-16", "2001-05-16", "2001-05-01", "2001-05-01"))
  expect_equal(date_by_intersecting(x, y), c(0, 1 / 7, 1 / 7, NA, 0))
  expect_equal(date_by_intersecting(x, y, within = 1), c(1, 3 / 7, 3 / 7, NA, 0))
  # recycling, as in date_is_intersecting()
  expect_equal(date_by_intersecting(x[2:3], as_messydate("2001-05-16")), c(1 / 7, 1 / 7))
  # a set and a month list their days
  expect_equal(date_by_intersecting(as_messydate("{2001-05-01,2001-05-10}"),
                                      as_messydate("2001-05-02"), within = 1), 1 / 2)
  expect_equal(date_by_intersecting(as_messydate("2001-05"), as_messydate("2001-05-31")), 1 / 31)
})

test_that("date_by_intersecting() agrees with a count of all day pairs", {
  set.seed(1)
  lo1 <- as.Date("2001-01-01") + sample(0:60, 40, TRUE)
  hi1 <- lo1 + sample(0:20, 40, TRUE)
  lo2 <- as.Date("2001-01-01") + sample(0:60, 40, TRUE)
  hi2 <- lo2 + sample(0:20, 40, TRUE)
  rng <- function(lo, hi) as_messydate(ifelse(lo == hi, format(lo), paste0(lo, "..", hi)))
  x <- rng(lo1, hi1); y <- rng(lo2, hi2)
  for (tau in c(0, 1, 3, 10)) {
    brute <- mapply(function(a1, b1, a2, b2) {
      d <- outer(seq(a1, b1), seq(a2, b2), function(u, v) abs(u - v) <= tau)
      mean(d)
    }, as.numeric(lo1), as.numeric(hi1), as.numeric(lo2), as.numeric(hi2))
    expect_equal(date_by_intersecting(x, y, within = tau), brute)
    expect_equal(date_is_intersecting(x, y, within = tau), brute > 0)
  }
})

test_that("date_by_intersecting() weights approximate dates", {
  a <- "2001-05-16~"
  b <- "2001-05-18"
  m <- messydates:::.weighted_members(a, 3)
  d <- as.numeric(as.Date(m$dates))
  w_in <- sum(m$w[abs(d - as.numeric(as.Date(b))) <= 1])
  expect_equal(date_by_intersecting(as_messydate(a), as_messydate(b),
                                      within = 1, approx_range = 3),
               w_in / sum(m$w))
  # without approx_range the mark is ignored
  expect_equal(date_by_intersecting(as_messydate(a), as_messydate(b), within = 1), 0)
})
