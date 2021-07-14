test_that("extract functions work properly", {
  expect_equal(unclass(year(as_messydate("2012-02-03"))), 2012)
  expect_equal(unclass(month(as_messydate("2012-02-03"))), 2)
  expect_equal(unclass(day(as_messydate("2012-02-03"))), 3)
})
