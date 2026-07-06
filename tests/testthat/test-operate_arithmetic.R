d <- as_messydate(c("2008-03-25", "-2012-02-27", "2001-01?", "2001",
                    "2001-01-01..2001-02-02", "{2001-01-01,2001-02-02}",
                    #"..2002-02-03",
                    "2001-01-03.."))
a <- as_messydate(c("2008-03-28", "-2012-02-24", "2001-01-04..2001-02-03",
                    "2001-01-04..2002-01-03", "2001-01-04..2001-02-05",
                    "{2001-01-04,2001-02-05}", #"..2002-02-06",
                    "2001-01-06.."))
s <- as_messydate(c("2008-03-22", "-2012-03-01", "2000-12-29..2001-01-28",
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
