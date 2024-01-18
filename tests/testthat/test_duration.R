test_that("mdates_duration class works", {
  expect_equal(class(messyduration("2010-01..2010-12")), "mdates_duration")
  expect_message(messyduration("2010-01..2010-12"), "Converting to mdate class.")
  expect_error(messyduration(as_messydate(c("2010-01-01", "2010-01-01"))),
               "mdates_duration class objects should have at least one date range")
  expect_equal(messyduration(as_messydate("2010-01..2010-12")),
               messyduration("2010-01-01..2010-12-31"))
  expect_equal(messyduration(as_messydate("2010-01..2010-12"),
                             approx_range = 1),
               messyduration("2010-01-02..2011-01-01"))
  expect_equal(messyduration(as_messydate("2010-01..2010-12"),
                             approx_range = -1),
               messyduration("2009-12-31..2010-12-30"))
})
