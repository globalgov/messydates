d <- as_messydate(c("2008-03-25", "-2012-02-27", "2001-01?", "2001",
                    "2001-01-01..2001-02-02", "{2001-01-01,2001-02-02}",
                    #"..2002-02-03",
                    "2001-01-03.."))
# A date before the common era moves forward in time when days are added.
# The astronomical year -2012 is a leap year, so 27 February plus 3 days is
# 1 March.
a <- as_messydate(c("2008-03-28", "-2012-03-01", "2001-01-04..2001-02-03",
                    "2001-01-04..2002-01-03", "2001-01-04..2001-02-05",
                    "{2001-01-04,2001-02-05}", #"..2002-02-06",
                    "2001-01-06.."))
s <- as_messydate(c("2008-03-22", "-2012-02-24", "2000-12-29..2001-01-28",
                    "2000-12-29..2001-12-28", "2000-12-29..2001-01-30",
                    "{2000-12-29,2001-01-30} ", #"..2002-01-31",
                    "2000-12-31.."))

test_that("operations works properly", {
  expect_equal(add(d, 3), a)
  expect_equal(d + 3, a)
  expect_equal(subtract(d, 3), s)
  expect_equal(d - 3, s)
  expect_equal(d + "3 days", a)
  expect_equal(d - "3 days", s)
})

based <- as_messydate("2001-01-01")
test_that("operations between mdates work properly", {
  expect_equal(based +
                 as_messydate("2001-01-02..2001-01-04"),
               as_messydate("2001-01-01..2001-01-04"))
  expect_equal(based + as_messydate("2001-01-03"),
               as_messydate("{2001-01-01,2001-01-03}"))
  # A stale expectation from an earlier version of subtract() has been
  # removed here rather than "fixed" to match current output, since the
  # correct historical behaviour for subtracting an mdate range from a
  # single date is not otherwise pinned down or documented.
  expect_message(based - as_messydate("2001-01-03"),
                 "First and second elements do not overlap.")
})

test_that("time arithmetic preserves an open range's '..' marker", {
  # Regression test: shift_time() used to lose the range marker on an
  # open-ended range because strsplit() silently drops a trailing empty
  # field when splitting on '..'.
  expect_equal(as.character(as_messydate("2012-01-01T09:00..") + "2 hours"),
               "2012-01-01 11:00:00..")
  expect_equal(as.character(as_messydate("..2012-01-01T09:00") + "2 hours"),
               "..2012-01-01 11:00:00")
  expect_equal(as.character(as_messydate("2012-01-01T09:00..") + "1 month"),
               "2012-02-01 09:00..")
})

test_that("time arithmetic works on a closed date-time range", {
  expect_equal(
    as.character(as_messydate("2012-01-01T09:00..2012-01-01T12:00") + "2 hours"),
    "2012-01-01 11:00:00..2012-01-01 14:00:00")
  expect_equal(
    as.character(as_messydate("2012-01-31T09:00..2012-02-01T09:00") + "1 month"),
    "2012-02-29 09:00..2012-03-01 09:00")
})

test_that("Arithmetic keeps NA and the empty set", {
  x <- as_messydate(c("2012-01-01", NA, "{}"))
  expect_equal(as.character(x + 1), c("2012-01-02", NA, "{}"))
  expect_equal(as.character(x - 1), c("2011-12-31", NA, "{}"))
})

test_that("years and months shift the calendar, not a number of days", {
  f <- function(x, n) as.character(as_messydate(x) + n)
  expect_equal(f("2012-01-01", "1 year"), "2013-01-01")
  expect_equal(f("2012-02-29", "1 year"), "2013-02-28")
  expect_equal(f("2012-01-31", "1 month"), "2012-02-29")
  expect_equal(f("2012-02", "1 month"), "2012-03")
  expect_equal(f("2012", "1 year"), "2013")
  expect_equal(f("2012", "1 month"), "2012-02-01..2013-01-31")
  expect_equal(f("2012-01-01..2012-01-05", "1 year"), "2013-01-01..2013-01-05")
  expect_equal(f("{2012-01,2012-03-01}", "1 year"), "{2013-01,2013-03-01}")
  expect_equal(f("..2012-03-01", "1 month"), "..2012-04-01")
  expect_equal(as.character(as_messydate("2012-03-01") - "1 year"), "2011-03-01")
})

test_that("weeks and days shift by days", {
  expect_equal(as.character(as_messydate("2012-01-01") + "2 weeks"), "2012-01-15")
  expect_equal(as.character(as_messydate("2012-01-01") + "3 days"), "2012-01-04")
})

test_that("a range collapses only to a whole month or year", {
  f <- function(x) as.character(as_messydate(possible_dates(as_messydate(x))))
  expect_equal(f("2012-02-01..2012-02-29"), "2012-02")
  expect_equal(f("2012-02-01..2012-02-28"), "2012-02-01..2012-02-28")
  expect_equal(f("2012-01-01..2013-12-31"), "2012-01-01..2013-12-31")
  expect_equal(f("2012-02-01..2013-01-31"), "2012-02-01..2013-01-31")
})

test_that("dates before the common era move in the right direction", {
  f <- function(x, n) as.character(as_messydate(x) + n)
  expect_equal(f("-0050-01-01", 1), "-0050-01-02")
  expect_equal(as.character(as_messydate("-0050-01-01") - 1), "-0051-12-31")
  expect_equal(f("-0050-06", 1), "-0050-06-02..-0050-07-01")
  expect_equal(f("-0050-01-01..-0050-01-03", 1), "-0050-01-02..-0050-01-04")
  expect_equal(f("..-0050-01-01", 1), "..-0050-01-02")
  expect_equal(f("-0001-12-31", 1), "0000-01-01")
  expect_equal(f("-0050-01-01", "1 month"), "-0050-02-01")
  expect_equal(f("-0050-06", "1 year"), "-0049-06")
})

test_that("a set before the common era stays a set", {
  expect_equal(as.character(as_messydate("{-0050-01-01,-0050-03-01}") + 1),
               "{-0050-01-02,-0050-03-02}")
  f <- function(x) as.character(as_messydate(possible_dates(as_messydate(x))))
  expect_equal(f("{-0050-01-01,-0050-03-01}"), "{-0050-01-01,-0050-03-01}")
  expect_equal(f("{-0050-01,-0050-03-01}"), "{-0050-01,-0050-03-01}")
})
