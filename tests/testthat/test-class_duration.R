mdur <- make_messyduration("2010-01..2010-12")
test_that("mdates_duration class works", {
  expect_s3_class(mdur, "mduration")
  expect_message(make_messyduration("2010-01..2010-12"), "Converting to mdate class.")
  expect_error(make_messyduration(as_messydate(c("2010-01-01", "2010-01-01"))),
               "mduration class objects should have at least one date range")
  expect_equal(make_messyduration(as_messydate("2010-01..2010-12")),
               make_messyduration("2010-01-01..2010-12-31"))
  expect_equal(mdur,
               make_messyduration("2010-01-01..2010-12-31"))
  expect_equal(make_messyduration(as_messydate("2010-01..2010-12"),
                             approx_range = 1),
               make_messyduration("2009-12-31..2011-01-01"))
  expect_equal(make_messyduration(as_messydate("2010-01..2010-12"),
                             approx_range = -1),
               make_messyduration("2010-01-02..2010-12-30"))
})

test_that("print.mduration prints without erroring", {
  expect_output(print(mdur))
})
