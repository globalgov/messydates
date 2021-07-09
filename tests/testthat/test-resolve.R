test_that("Resolve dates works properly for date ranges", {
  range <- "2014-01-01..2014-01-31"
  expect_equal(as.character(min.messydt(as_messydate(range))), "2014-01-01")
  expect_equal(as.character(max.messydt(as_messydate(range))), "2014-01-31")
  expect_equal(as.character(median.messydt(as_messydate(range))), "2014-01-16")
  expect_equal(as.character(mean.messydt(as_messydate(range))), "2014-01-16")
  expect_equal(as.character(modal.messydt(as_messydate(range))), "2014-01-01") # get the first when no mode?
})

test_that("Resolve dates works properly for unspecified dates", {
  unespecified<- "1999"
  expect_equal(as.character(min.messydt(as_messydate(unspecified))), "1999-01-01")
  expect_equal(as.character(max.messydt(as_messydate(unspecified))), "1999-12-31")
  expect_equal(as.character(median.messydt(as_messydate(unspecified))), "1999-07-02")
  expect_equal(as.character(mean.messydt(as_messydate(unspecified))), "1999-07-02")
  expect_equal(as.character(modal.messydt(as_messydate(unspecified))), "1999-01-01") # get the first when no mode?
})
