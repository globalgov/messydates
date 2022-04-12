test_that("mutiple variables are properly bind", {
  expect_equal(make_messydate("2010", "1", "1"), as_messydate("2010-01-01"))
  expect_equal(make_messydate("2010", "XX", "10"), as_messydate("2010-XX-10"))
  expect_equal(make_messydate("2010-01-01"), as_messydate("2010-1-1"))
  expect_equal(make_messydate("2012?", "01", "01"), as_messydate("2012?-01-01"))
  expect_equal(make_messydate("2012", "01", "01~"), as_messydate("2012-01-01~"))
  expect_error(make_messydate(c("2010"), c("10")))
})
