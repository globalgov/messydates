dt <- as_messydate(c("2012-02-03T14:30:05", "2012-02-03", "2012-02", "2012"))

test_that("date components still extract correctly with a time present", {
  expect_equal(year(dt), c(2012L, 2012L, 2012L, 2012L))
  expect_equal(month(dt), c(2L, 2L, 2L, NA))
  expect_equal(day(dt), c(3L, 3L, NA, NA))
})

test_that("time components extract, NA when absent", {
  expect_equal(hour(dt), c(14L, NA, NA, NA))
  expect_equal(minute(dt), c(30L, NA, NA, NA))
  expect_equal(second(dt), c(5, NA, NA, NA))
})

test_that("fractional seconds return a numeric", {
  expect_equal(second(as_messydate("2012-02-03T14:30:05.5")), 5.5)
})

test_that("tz returns designator or offset", {
  expect_equal(tz(as_messydate("2012-02-03T14:30:00+02:00")), "+02:00")
  expect_equal(tz(as_messydate("2012-02-03T14:30:00Z")), "Z")
  expect_true(is.na(tz(as_messydate("2012-02-03T14:30:00"))))
})

test_that("precision extends below the day for times", {
  expect_equal(precision(as_messydate("2012-02-03T14")), 24)
  expect_equal(precision(as_messydate("2012-02-03T14:30")), 1440)
  expect_equal(precision(as_messydate("2012-02-03T14:30:05")), 86400)
})

test_that("precision is unchanged for date-only values", {
  expect_equal(precision(as_messydate("2012-02-03")), 1)
  expect_equal(precision(as_messydate("2012-02")), 0.03448276, tolerance = 0.001)
  expect_equal(precision(as_messydate("2012")), 0.00273224, tolerance = 0.001)
})
