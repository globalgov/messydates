mdatey <- as_messydate("2012-02-03")
test_that("extract functions work properly", {
  expect_equal(unclass(year(mdatey)), 2012)
  expect_equal(unclass(month(mdatey)), 2)
  expect_equal(unclass(day(mdatey)), 3)
})

test_that("precision function works properly", {
  expect_equal(precision(mdatey), 1)
  expect_equal(precision(as_messydate("2012-02-03?")), 1)
  expect_equal(precision(as_messydate("2012-02-~03")), 1)
  expect_equal(precision(as_messydate("2012-02-03..2012-02-14")), 0.0833,
               tolerance = 0.001)
  expect_equal(precision(as_messydate("2012-02")), 0.03448276,
               tolerance = 0.001)
  expect_equal(precision(as_messydate("2012")), 0.00273224,
               tolerance = 0.001)
})
