# Interoperability with base R, lubridate, and (where installed) anytime/clock.
# The component accessors are S3 methods on lubridate's generics, so they must
# extend (not mask) them: dispatch to the messy-date logic on `mdate`, and to
# lubridate's own methods on `Date`/`POSIXct`.

test_that("accessors dispatch to lubridate methods on Date/POSIXct", {
  d <- as.Date("2020-06-15")
  p <- as.POSIXct("2019-03-01 14:30:00", tz = "UTC")
  expect_equal(lubridate::year(d), 2020L)
  expect_equal(lubridate::month(d), 6L)
  expect_equal(lubridate::day(d), 15L)
  expect_equal(lubridate::hour(p), 14L)
  expect_equal(lubridate::minute(p), 30L)
  # tz() on a POSIXct still returns its Olson zone, not an mdate offset
  expect_equal(lubridate::tz(p), "UTC")
})

test_that("accessors dispatch to mdate methods on mdate, keeping precision", {
  expect_equal(year(as_messydate("2020-06-XX")), 2020L)
  expect_equal(month(as_messydate("2012-02")), 2L)
  expect_equal(day(as_messydate("2012-02-03")), 3L)
  expect_true(is.na(day(as_messydate("2012-02"))))
  # mday() (which day() dispatches through) also reaches the mdate method
  expect_equal(lubridate::mday(as_messydate("2012-02-09")), 9L)
  # tz() on an mdate returns the ISO offset designator
  expect_equal(tz(as_messydate("2012-02-03 14:30:00+02:00")), "+02:00")
  expect_true(is.na(tz(as_messydate("2012-02-03 14:30:00"))))
})

test_that("lubridate coercion verbs honour the mdate resolver", {
  md <- as_messydate("2012-06")
  expect_equal(lubridate::as_date(md), as.Date("2012-06-01"))
  expect_equal(lubridate::as_date(md, FUN = vmax), as.Date("2012-06-30"))
  # as_datetime() bypasses lubridate's tz(x) default and mirrors as.POSIXct()
  expect_equal(lubridate::as_datetime(as_messydate("2019-03-01 14:30:00")),
               as.POSIXct("2019-03-01 14:30:00", tz = "UTC"))
})

test_that("as.POSIXct()/as.POSIXlt() convert a vector of mdates", {
  md <- as_messydate(c("2012-06-15", "2019-03-01"))
  expect_s3_class(as.POSIXct(md), "POSIXct")
  expect_length(as.POSIXct(md), 2)
  expect_s3_class(as.POSIXlt(md), "POSIXlt")
  # BCE dates are still rejected (with a clear message), for the whole vector
  expect_error(as.POSIXct(as_messydate(c("2012-06-15", "-0500"))),
               "negative dates")
})

test_that("format() returns the ISO strings for data frame display", {
  md <- as_messydate(c("2012-06", "2012-06-01..2012-06-30"))
  # format() pads to a common width (as for any character vector); trimmed,
  # each element is its underlying ISO string.
  expect_equal(trimws(format(md)), c("2012-06", "2012-06-01..2012-06-30"))
  expect_equal(as.character(data.frame(d = md)$d), unclass(md))
})

test_that("base Date round-trips through mdate unchanged", {
  d <- as.Date(c("2012-06-15", "1999-12-31"))
  expect_equal(as.Date(as_messydate(d)), d)
})

test_that("anytime output coerces to mdate", {
  skip_if_not_installed("anytime")
  expect_equal(as.character(as_messydate(anytime::anydate("2012-06-01"))),
               "2012-06-01")
})

test_that("clock round-trips through mdate via Date/POSIXct", {
  skip_if_not_installed("clock")
  md <- as_messydate("2012-06-15")
  expect_equal(clock::as_date(as.Date(md)), clock::date_build(2012, 6, 15))
  cd <- clock::date_build(2012, 6, 15)
  expect_equal(as.character(as_messydate(as.Date(cd))), "2012-06-15")
})
