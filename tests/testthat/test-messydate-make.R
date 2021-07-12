test_that("mutiple variables are properly bind", {
  expect_equal(make_messydate("2010", "10", "10"), "2010-10-10")
  expect_equal(make_messydate("2010", "10", "NA"), "2010-10-NA")
  expect_equal(make_messydate("2010-10-10"), "2010-10-10")
  expect_equal(make_messydate("2012?","01","01"), "2012?-01-01")
  expect_equal(make_messydate("2012","01","01~"), "2012-01-01~")
})
