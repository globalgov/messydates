test_that("precise date-times pass through expansion keeping their time", {
  expect_equal(unlist(compatible_dates(as_messydate("2012-02-03 14:30:05"))),
               "2012-02-03 14:30:05")
})

test_that("date-time ranges expand at day granularity by default (time dropped)", {
  expect_equal(unlist(compatible_dates(as_messydate("2019-03-01 09:00..2019-03-01 17:00"))),
               "2019-03-01")
  expect_equal(unlist(compatible_dates(as_messydate("2019-03-01 09:00..2019-03-03 17:00"))),
               c("2019-03-01", "2019-03-02", "2019-03-03"))
})

test_that("the deprecated expand() keeps a sub-day 'by' for date-time ranges", {
  expect_equal(
    unlist(.compatible(as_messydate("2019-03-01 09:00..2019-03-01 12:00"), by = "hour")),
    c("2019-03-01 09:00:00", "2019-03-01 10:00:00",
      "2019-03-01 11:00:00", "2019-03-01 12:00:00"))
})

test_that("a sub-day 'by' falls back to normal expansion for a non-range value", {
  expect_equal(unlist(.compatible(as_messydate("2019-03-01 09:00"), by = "hour")),
               "2019-03-01 09:00")
  expect_equal(unlist(.compatible(as_messydate("2019-03-01"), by = "hour")),
               "2019-03-01")
})

test_that("a bare time (no date part) expands to itself", {
  expect_equal(unlist(compatible_dates(as_messydate("14:30"))), "14:30")
  expect_equal(unlist(compatible_dates(as_messydate("14:30:05"))), "14:30:05")
  # even when mixed with a date range, order is preserved and each is handled
  expect_equal(
    compatible_dates(as_messydate(c("14:30", "2012-01-01..2012-01-02", "2pm"))),
    list("14:30", c("2012-01-01", "2012-01-02"), "14:00"))
})

test_that("date-only expansion is unaffected by the new time handling", {
  expect_equal(unlist(compatible_dates(as_messydate("2012-02"))),
               as.character(seq(as.Date("2012-02-01"), as.Date("2012-02-29"),
                                by = "day")))
})

test_that("expand by hour keeps the UTC offset of a date-time range", {
  expect_equal(.compatible(as_messydate("2019-03-01 09:00+02:00..2019-03-01 11:00+02:00"),
                      by = "hour")[[1]],
               c("2019-03-01 09:00:00+02:00", "2019-03-01 10:00:00+02:00",
                 "2019-03-01 11:00:00+02:00"))
  expect_equal(.compatible(as_messydate("2019-03-01 09:00Z..2019-03-01 10:00Z"),
                      by = "hour")[[1]],
               c("2019-03-01 09:00:00Z", "2019-03-01 10:00:00Z"))
  # ends with different offsets are sequenced as instants, in the first offset
  expect_equal(.compatible(as_messydate("2019-03-01 09:00+02:00..2019-03-01 09:00Z"),
                      by = "hour")[[1]],
               c("2019-03-01 09:00:00+02:00", "2019-03-01 10:00:00+02:00",
                 "2019-03-01 11:00:00+02:00"))
})
