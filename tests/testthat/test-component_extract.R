mdatey <- as_messydate("2012-02-03")
test_that("extract functions work properly", {
  expect_equal(unclass(year(mdatey)), 2012)
  expect_equal(unclass(month(mdatey)), 2)
  expect_equal(unclass(day(mdatey)), 3)
})

test_that("precision function works properly", {
  expect_equal(precision(mdatey), 1)
  expect_equal(precision(as_messydate("2012-02-03?")), 1)
  # expect_equal(precision(as_messydate("2012-02-~03")), 1)
  expect_equal(precision(as_messydate("2012-02-03..2012-02-14")), 12)
  # expect_equal(precision(as_messydate("2012-02")), 29)
  # expect_equal(precision(as_messydate("2012")), 366)
})
