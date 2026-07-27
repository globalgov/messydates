test_that("set functions work properly", {
  expect_equal(unclass(as_messydate("2012-01-01..2012-01-03") %union%
                         as_messydate("2012-01-03..2012-01-04")),
               c("2012-01-01", "2012-01-02", "2012-01-03", "2012-01-04"))
  expect_equal(unclass(as_messydate("2012-01-01..2012-01-03") %intersect%
                                    as_messydate("2012-01-02")),
               "2012-01-02")
})

test_that("set functions return an empty vector when there is no overlap", {
  expect_length(as_messydate("2012-01-01") %intersect% as_messydate("2012-02-01"), 0)
})

test_that("set functions dispatch for Date and POSIXt", {
  expect_equal(as.Date("2012-01-02") %intersect% as_messydate("2012-01-01..2012-01-03"),
               "2012-01-02")
  expect_equal(as.POSIXct("2012-01-02", tz = "UTC") %intersect%
                 as_messydate("2012-01-01..2012-01-03"),
               "2012-01-02")
  expect_equal(as.Date("2012-01-02") %union% as_messydate("2012-01-03"),
               c("2012-01-02", "2012-01-03"))
  expect_equal(as.POSIXct("2012-01-02", tz = "UTC") %union% as_messydate("2012-01-03"),
               c("2012-01-02", "2012-01-03"))
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
  expect_equal(expand(onesie)[[1]], c("2019-01-01", "2019-02-02"))
  expect_equal(unclass(vmin(onesie)), "2019-01-01")
  expect_equal(unclass(vmax(onesie)), "2019-02-02")
})
