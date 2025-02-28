test_that("set functions work properly", {
  expect_equal(unclass(as_messydate("2012-01-01..2012-01-03") %union%
                         as_messydate("2012-01-03..2012-01-04")),
               c("2012-01-01", "2012-01-02", "2012-01-03", "2012-01-04"))
  expect_equal(unclass(as_messydate("2012-01-01..2012-01-03") %intersect%
                                    as_messydate("2012-01-02")),
               "2012-01-02")
})
