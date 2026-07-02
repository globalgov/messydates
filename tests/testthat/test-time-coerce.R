test_that("as.POSIXct keeps the time of day", {
  expect_equal(as.POSIXct(as_messydate("2012-02-03T14:30:00"), FUN = vmin),
               as.POSIXct("2012-02-03 14:30:00", tz = "UTC"))
})

test_that("as.POSIXct honours a UTC offset", {
  expect_equal(as.POSIXct(as_messydate("2012-02-03T14:30:00+02:00"), FUN = vmin),
               as.POSIXct("2012-02-03 12:30:00", tz = "UTC"))
})

test_that("as.POSIXlt keeps the time of day", {
  expect_equal(as.POSIXlt(as_messydate("2012-02-03T14:30:00"), FUN = vmin),
               as.POSIXlt("2012-02-03 14:30:00", tz = "UTC"))
})

test_that("date-only coercion is unchanged", {
  messy <- as_messydate("2010-10-10..2010-10-20")
  expect_equal(as.POSIXct(messy, FUN = vmax),
               as.POSIXct("2010-10-20", tz = "UTC"))
})

test_that("arithmetic in sub-day units shifts the time", {
  expect_equal(as.character(as_messydate("2012-02-03T14:30:00") + "2 hours"),
               "2012-02-03T16:30:00")
  expect_equal(as.character(as_messydate("2012-02-03T00:30:00") - "90 minutes"),
               "2012-02-02T23:00:00")
  expect_equal(as.character(as_messydate("2012-02-03T14:30:00") + "30 seconds"),
               "2012-02-03T14:30:30")
})

test_that("numeric arithmetic on a date-time adds days, keeping the time", {
  expect_equal(as.character(as_messydate("2012-02-03T14:30:00") + 1),
               "2012-02-04T14:30:00")
})

test_that("sub-day arithmetic promotes a date-only value", {
  expect_equal(as.character(as_messydate("2012-02-03") + "2 hours"),
               "2012-02-03T02:00:00")
})

test_that("arithmetic preserves a UTC offset", {
  expect_equal(
    as.character(as_messydate("2012-02-03T14:30:00+02:00") + "30 seconds"),
    "2012-02-03T14:30:30+02:00")
})

test_that("seq works at sub-day steps", {
  expect_equal(
    seq(as_messydate("2019-03-01T09:00"), as_messydate("2019-03-01T12:00"),
        by = "hour"),
    c("2019-03-01T09:00:00", "2019-03-01T10:00:00",
      "2019-03-01T11:00:00", "2019-03-01T12:00:00"))
})

test_that("as.Date drops the time of day", {
  expect_equal(as.Date(as_messydate("2012-01-01T14:30"), FUN = vmin),
               as.Date("2012-01-01"))
  expect_equal(as.Date(as_messydate("2012-01-01T~14:30"), FUN = vmax),
               as.Date("2012-01-01"))
})

test_that("messyduration keeps sub-day precision", {
  expect_equal(
    as.character(messyduration(
      as_messydate("2010-01-01T09:00..2010-01-01T17:00"))),
    "2010-01-01T09:00:00..2010-01-01T17:00:00")
})
