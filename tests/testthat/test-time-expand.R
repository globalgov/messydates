test_that("precise date-times pass through expansion keeping their time", {
  expect_equal(unlist(expand(as_messydate("2012-02-03 14:30:05"))),
               "2012-02-03 14:30:05")
})

test_that("date-time ranges expand at day granularity by default (time dropped)", {
  expect_equal(unlist(expand(as_messydate("2019-03-01 09:00..2019-03-01 17:00"))),
               "2019-03-01")
  expect_equal(unlist(expand(as_messydate("2019-03-01 09:00..2019-03-03 17:00"))),
               c("2019-03-01", "2019-03-02", "2019-03-03"))
})

test_that("sub-day 'by' opts into finer enumeration of date-time ranges", {
  expect_equal(
    unlist(expand(as_messydate("2019-03-01 09:00..2019-03-01 12:00"), by = "hour")),
    c("2019-03-01 09:00:00", "2019-03-01 10:00:00",
      "2019-03-01 11:00:00", "2019-03-01 12:00:00"))
})

test_that("a sub-day 'by' falls back to normal expansion for a non-range value", {
  expect_equal(unlist(expand(as_messydate("2019-03-01 09:00"), by = "hour")),
               "2019-03-01 09:00")
  expect_equal(unlist(expand(as_messydate("2019-03-01"), by = "hour")),
               "2019-03-01")
})

test_that("date-only expansion is unaffected by the new time handling", {
  expect_equal(unlist(expand(as_messydate("2012-02"))),
               as.character(seq(as.Date("2012-02-01"), as.Date("2012-02-29"),
                                by = "day")))
})
