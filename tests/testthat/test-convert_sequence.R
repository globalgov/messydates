test_that("seq.mdate works for a straightforward CE sequence", {
  expect_equal(seq(as_messydate("2012-01-01"), as_messydate("2012-01-05")),
               seq(as.Date("2012-01-01"), as.Date("2012-01-05"), by = "days"))
})

test_that("seq.mdate uses the min/max of a range when 'to' is missing", {
  expect_equal(seq(as_messydate("2012-01-01..2012-01-05")),
               seq(as.Date("2012-01-01"), as.Date("2012-01-05"), by = "days"))
})

test_that("seq.mdate works within a single BCE year", {
  expect_equal(seq(as_messydate("-0010-01-01"), as_messydate("-0010-01-05")),
               c("-0010-01-01", "-0010-01-02", "-0010-01-03", "-0010-01-04",
                 "-0010-01-05"))
})

test_that("seq.mdate works across multiple BCE years", {
  r <- seq(as_messydate("-0012-12-28"), as_messydate("-0010-01-03"))
  expect_length(r, 372)
  expect_equal(r[1:3], c("-0012-12-28", "-0012-12-29", "-0012-12-30"))
  expect_equal(tail(r, 3), c("-0010-01-01", "-0010-01-02", "-0010-01-03"))
})

test_that("seq.mdate works across a BCE gap spanning several whole middle years", {
  r <- seq(as_messydate("-0006-01-01"), as_messydate("-0001-01-05"))
  expect_length(r, 1831)
  expect_equal(r[1:3], c("-0006-01-01", "-0006-01-02", "-0006-01-03"))
  expect_equal(tail(r, 3), c("-0001-01-03", "-0001-01-04", "-0001-01-05"))
})

test_that("seq.mdate crosses the BCE/CE boundary through astronomical year zero", {
  # ISO 8601-2 numbers years astronomically, so a year zero (= 1 BCE) sits
  # between -0001 and 0001; the sequence must pass through the whole of it
  # (a 366-day leap year) rather than jumping straight from -0001 to 0001.
  r <- seq(as_messydate("-0001-12-28"), as_messydate("0001-01-05"))
  expect_length(r, 375)
  # end of year -1, then straight into year 0 (not year 1)
  expect_equal(head(r, 5),
               c("-0001-12-28", "-0001-12-29", "-0001-12-30", "-0001-12-31",
                 "0000-01-01"))
  expect_equal(tail(r, 3), c("0001-01-03", "0001-01-04", "0001-01-05"))
  # year 0 is a full 366-day leap year, including 29 February
  expect_equal(sum(grepl("^0000-", r)), 366L)
  expect_true(all(c("0000-02-29", "0000-12-31", "0001-01-01") %in% r))
})

test_that("seq.mdate accepts a non-day 'by'", {
  expect_equal(seq(as_messydate("2012-01-01"), as_messydate("2012-03-01"), by = "month"),
               seq(as.Date("2012-01-01"), as.Date("2012-03-01"), by = "month"))
})

test_that("seq.mdate supports sub-day sequences via a time-of-day 'by'", {
  expect_equal(
    seq(as_messydate("2019-03-01T09:00"), as_messydate("2019-03-01T12:00"),
        by = "hour"),
    c("2019-03-01 09:00:00", "2019-03-01 10:00:00",
      "2019-03-01 11:00:00", "2019-03-01 12:00:00"))
})

test_that("seq.mdate uses POSIXct once either endpoint carries a time, even with a day-based by", {
  r <- seq(as_messydate("2019-03-01T00:00:00"), as_messydate("2019-03-03T00:00:00"),
           by = "day")
  expect_equal(r, c("2019-03-01 00:00:00", "2019-03-02 00:00:00",
                     "2019-03-03 00:00:00"))
})
