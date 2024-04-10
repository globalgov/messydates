date <- as.Date("2010-10-10")
POSIXct <- as.POSIXct("2010-10-20 CEST")
POSIXlt <- as.POSIXlt("2010-10-15 CEST")
messy <- as_messydate("2010-10-10..2010-10-20")
negative <- min(as_messydate("1000 BC"))

test_that("Coercion from other date classes into messydt works", {
  expect_equal(as.character(as.Date(as_messydate("1000 BC"), max)), "-1000-12-31")
  expect_equal(as.Date(messy, min), date)
  expect_equal(as.POSIXct(messy, max), POSIXct)
  expect_equal(as.POSIXlt(messy, mean), POSIXlt)
  expect_equal(as.Date(as_messydate("2010-10-10"), median), date)
  expect_equal(as.Date(as_messydate("2010-10-10"), random), date)
  expect_equal(as.character(as.Date(as_messydate("1000 BC"), min)), min(negative))
  expect_error(as.POSIXct(as_messydate("-2012"), min))
  expect_error(as.POSIXlt(as_messydate("-2012"), min))
})

test_that("Coercion from other types of negative dates work", {
  expect_equal(as.character(as.Date(as_messydate(c("-27", "-14")), min)),
               c("-27-01-01", "-14-01-01"))
  expect_equal(max(as_messydate(c("-27", "-14"))),
               c("-0027-12-31", "-0014-12-31"))
  expect_equal(mean(as_messydate(c("-27", "-14"))),
               c("-0027-07-02", "-0014-07-02"))
})
