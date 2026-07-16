test_that("ISO 8601-2 times parse and round-trip, with a space separator", {
  expect_equal(as.character(as_messydate("2019-03-01 14:30:00Z")),
               "2019-03-01 14:30:00Z")
  expect_equal(as.character(as_messydate("2019-03-01 14:30:00")),
               "2019-03-01 14:30:00")
  expect_equal(as.character(as_messydate("2019-03-01 14:30")),
               "2019-03-01 14:30")
  expect_equal(as.character(as_messydate("2019-03-01 14")),
               "2019-03-01 14")
})

test_that("'T' is still accepted on input and normalised to a space", {
  expect_equal(as_messydate("2019-03-01T14:30:00Z"),
               as_messydate("2019-03-01 14:30:00Z"))
  expect_equal(as.character(as_messydate("2019-03-01T14:30:00")),
               "2019-03-01 14:30:00")
})

test_that("time components are zero-padded", {
  expect_equal(as.character(as_messydate("2019-03-01 9:5:3")),
               "2019-03-01 09:05:03")
  # An hour with nothing following it is only recognised as a time when
  # introduced by 'T': a bare "space + 1-2 digits" is deliberately not
  # treated as a time on its own, since that would misread the space
  # before a second date in a plain set/list (e.g. "2012-01-01, 2012-02-02")
  # as introducing a fake time.
  expect_equal(as.character(as_messydate("2019-03-01T9")),
               "2019-03-01 09")
})

test_that("fractional seconds are preserved", {
  expect_equal(as.character(as_messydate("2019-03-01 14:30:00.5")),
               "2019-03-01 14:30:00.5")
})

test_that("am/pm times convert to 24-hour", {
  expect_equal(as.character(as_messydate("2019-03-01 2:30pm")),
               "2019-03-01 14:30")
  expect_equal(as.character(as_messydate("2019-03-01 12:00am")),
               "2019-03-01 00:00")
  expect_equal(as.character(as_messydate("2019-03-01 12:00pm")),
               "2019-03-01 12:00")
})

test_that("UTC offsets are normalised, UTC written as Z", {
  expect_equal(as.character(as_messydate("2019-03-01 14:30:00+02:00")),
               "2019-03-01 14:30:00+02:00")
  expect_equal(as.character(as_messydate("2019-03-01 14:30:00-0500")),
               "2019-03-01 14:30:00-05:00")
  expect_equal(as.character(as_messydate("2019-03-01 14:30:00+0000")),
               "2019-03-01 14:30:00Z")
})

test_that("time-of-day annotations are carried through", {
  expect_equal(as.character(as_messydate("2019-03-01 14:30~")),
               "2019-03-01 14:30~")
  expect_equal(as.character(as_messydate("2019-03-01 14:30?")),
               "2019-03-01 14:30?")
  # an annotation on the hour or minute is recognised whether the input
  # uses a space or 'T' as the date-time separator
  expect_equal(as.character(as_messydate("2019-03-01 ~14:30")),
               "2019-03-01 ~14:30")
  expect_equal(as.character(as_messydate("2019-03-01T~14:30")),
               "2019-03-01 ~14:30")
  expect_equal(as.character(as_messydate("2019-03-01 14:~30")),
               "2019-03-01 14:~30")
})

test_that("date-time ranges parse on both operands", {
  expect_equal(as.character(as_messydate("2019-03-01 09:00..2019-03-01 17:00")),
               "2019-03-01 09:00..2019-03-01 17:00")
})

test_that("times attach to reduced-precision dates", {
  expect_equal(as.character(as_messydate("2019 14:30")), "2019 14:30")
})

test_that("a time with no date part parses on its own", {
  expect_equal(as.character(as_messydate("14:30")), "14:30")
  expect_equal(as.character(as_messydate("14:30:05")), "14:30:05")
  expect_equal(as.character(as_messydate("9:5:3")), "09:05:03")
  expect_equal(as.character(as_messydate("14:30:00.5")), "14:30:00.5")
  # am/pm without a date; a bare am/pm hour fills the minutes to ":00"
  expect_equal(as.character(as_messydate("2:30pm")), "14:30")
  expect_equal(as.character(as_messydate("2pm")), "14:00")
  expect_equal(as.character(as_messydate("2 PM")), "14:00")
  expect_equal(as.character(as_messydate("12am")), "00:00")
  expect_equal(as.character(as_messydate("12pm")), "12:00")
  # offsets and the Z designator work without a date too
  expect_equal(as.character(as_messydate("14:30Z")), "14:30Z")
  expect_equal(as.character(as_messydate("14:30:00-0500")), "14:30:00-05:00")
})

test_that("a bare time carries the same annotations as a date-time", {
  expect_equal(as.character(as_messydate("14:30~")), "14:30~")
  expect_equal(as.character(as_messydate("~14:30")), "~14:30")
  expect_equal(as.character(as_messydate("14:~30")), "14:~30")
  expect_equal(as.character(as_messydate("14:30%")), "14:30%")
  expect_equal(as.character(as_messydate("XX:30")), "XX:30")
  expect_equal(as.character(as_messydate("14:XX")), "14:XX")
})

test_that("a qualifier applies to a bare time (prose)", {
  expect_equal(as.character(as_messydate("around 2pm")), "14:00~")
  expect_equal(as.character(as_messydate("about 14:30")), "14:30~")
  expect_equal(as.character(as_messydate("maybe 3:15pm")), "15:15?")
  expect_equal(as.character(as_messydate("perhaps around 2pm")), "14:00%")
})

test_that("a leading 'at' before a bare time is dropped", {
  expect_equal(as.character(as_messydate("at 2:30pm")), "14:30")
  expect_equal(as.character(as_messydate("at 14:30")), "14:30")
  expect_equal(as.character(as_messydate("at around 2pm")), "14:00~")
  expect_equal(as.character(as_messydate("at around 2:30pm")), "14:30~")
  expect_equal(as.character(as_messydate("at possibly 2:30pm")), "14:30?")
})

test_that("bare-time detection does not misread date sets/ranges", {
  # a colon between two 4-digit years is still a range, not a time
  expect_equal(as.character(as_messydate("2009:2019")), "2009..2019")
  # a comma-separated list of dates is unaffected
  expect_equal(as.character(as_messydate("2012-01-01, 2012-02-02")),
               "{2012-01-01,2012-02-02}")
  # a lone hour is read as a year, not a time (no strong time signal)
  expect_equal(as.character(as_messydate("2019")), "2019")
})

test_that("coercion from POSIXct/POSIXlt preserves time but drops midnight", {
  expect_equal(as.character(as_messydate(as.POSIXct("2010-10-10", tz = "UTC"))),
               "2010-10-10")
  expect_equal(
    as.character(as_messydate(as.POSIXct("2010-10-10 14:30:00", tz = "UTC"))),
    "2010-10-10 14:30:00Z")
  expect_equal(
    as.character(as_messydate(as.POSIXlt("2010-10-10 14:30:00", tz = "UTC"))),
    "2010-10-10 14:30:00Z")
})

test_that("validation accepts time characters and rejects other letters", {
  expect_silent(validate_messydate(as_messydate("2019-03-01 14:30:00Z")))
  expect_silent(validate_messydate(as_messydate("2019-03-01 14:30:00+02:00")))
  expect_error(validate_messydate(new_messydate("2019-03-01G")))
})

test_that("datetimes are precise, partial dates are not", {
  expect_true(is_precise(as_messydate("2019-03-01 14:30:00")))
  expect_true(is_precise(as_messydate("2019-03-01")))
  expect_false(is_precise(as_messydate("2019-03")))
})
