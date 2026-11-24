test_that("set functions return the shortest mdate", {
  expect_equal(as_messydate("2012-01-01..2012-01-03") %union%
                 as_messydate("2012-01-03..2012-01-04"),
               as_messydate("2012-01-01..2012-01-04"))
  expect_equal(as_messydate("2012-01-01..2012-01-03") %intersect%
                 as_messydate("2012-01-02"),
               as_messydate("2012-01-02"))
  expect_equal(as_messydate("2012-01") %intersect%
                 as_messydate("2012-01-10..2012-02-05"),
               as_messydate("2012-01-10..2012-01-31"))
  expect_equal(as_messydate("2012-01-01") %union% as_messydate("2012-02-01"),
               as_messydate("{2012-01-01,2012-02-01}"))
})

test_that("set functions work element by element, with recycling", {
  x <- as_messydate(c("2012-01-01..2012-01-05", "2012-02", "2012-03-01"))
  expect_equal(as.character(x %intersect% as_messydate("2012-01-04..2012-02-02")),
               c("2012-01-04..2012-01-05", "2012-02-01..2012-02-02", "{}"))
  expect_length(x %union% x, 3)
  expect_error(x %union% x[1:2])
})

test_that("set functions give {} for no overlap and NA for a missing value", {
  expect_equal(as.character(as_messydate("2012-01-01") %intersect%
                              as_messydate("2012-02-01")), "{}")
  expect_true(date_is_empty(as_messydate("2012-01-01") %intersect%
                              as_messydate("2012-02-01")))
  expect_equal(as.character(as_messydate(c(NA, "2012-01-01")) %union%
                              as_messydate("2012-01-02")),
               c(NA, "2012-01-01..2012-01-02"))
})

test_that("set functions carry ? and ~ into the result", {
  expect_equal(as.character(as_messydate("2012-06?") %intersect%
                              as_messydate("2012-06-10..2012-07-05")),
               "2012-06-10?..2012-06-30?")
  expect_true(date_is_approximate(as_messydate("~2012-01-01..2012-01-05") %union%
                                    as_messydate("2012-01-03")))
})

test_that("set functions sort dates before the common era by time", {
  expect_equal(as.character(as_messydate("-0050-01-01..-0050-01-05") %intersect%
                              as_messydate("-0050-01-03..-0050-01-10")),
               "-0050-01-03..-0050-01-05")
})

test_that("set functions dispatch for Date and POSIXt", {
  expect_equal(as.Date("2012-01-02") %intersect% as_messydate("2012-01-01..2012-01-03"),
               as_messydate("2012-01-02"))
  expect_equal(as.POSIXct("2012-01-02", tz = "UTC") %intersect%
                 as_messydate("2012-01-01..2012-01-03"),
               as_messydate("2012-01-02"))
  expect_equal(as.Date("2012-01-02") %union% as_messydate("2012-01-03"),
               as_messydate("2012-01-02..2012-01-03"))
})

test_that("'[]' sets are distinguished from '{}' sets", {
  # '[]' means one member of the set, '{}' all of them. The brackets used to
  # be stripped and restored as braces, losing the distinction.
  expect_equal(unclass(as_messydate("[2019-01-01,2019-02-02]")),
               "[2019-01-01,2019-02-02]")
  expect_equal(unclass(as_messydate("{2019-01-01,2019-02-02}")),
               "{2019-01-01,2019-02-02}")
  # Both enumerate and resolve to the same dates; they differ in meaning.
  onesie <- as_messydate("[2019-01-01,2019-02-02]")
  expect_equal(possible_dates(onesie)[[1]], c("2019-01-01", "2019-02-02"))
  expect_equal(unclass(vmin(onesie)), "2019-01-01")
  expect_equal(unclass(vmax(onesie)), "2019-02-02")
})
