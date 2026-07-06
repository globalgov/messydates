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
