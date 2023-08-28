test_that("Coercion from other date classes into messydt works", {
  date <- as.Date("2010-10-10")
  POSIXct <- as.POSIXct("2010-10-10")
  POSIXlt <- as.POSIXlt("2010-10-10")
  character <- "2010-10-10"
  character2 <- "AD2010-10-10"
  character3 <- "{BC2010-10-10,BC2010-10-11,BC2010-10-12}"
  month_text <- "10 October 2010"
  messy <- as_messydate("2010-10-10")
  messyneg <- as_messydate("{-2010-10-10,-2010-10-11,-2010-10-12}")
  expect_equal(as_messydate(date), messy)
  expect_equal(as_messydate(POSIXct), messy)
  expect_equal(as_messydate(POSIXlt), messy)
  expect_equal(as_messydate(character), messy)
  expect_equal(as_messydate(character2), messy)
  expect_equal(as_messydate(character3), messyneg)
  expect_equal(as_messydate(month_text), messy)
  expect_equal(mdate(date), messy)
  expect_equal(mdate(POSIXct), messy)
  expect_equal(mdate(POSIXlt), messy)
  expect_equal(mdate(character), messy)
  expect_equal(mdate(character2), messy)
  expect_equal(mdate(character3), messyneg)
  expect_equal(mdate(month_text), messy)
})

test_that("Coercion of unespecified date components are properly handled", {
  unspecified <- c("1908-??-??", "1908-10-??", "1908/X/X", "1908/?/?",  "XX-1998",
                   "XXXX-01-01", "01-01-XXXX", "XX-10-1998", "XX-XX-1998")
  b <- as_messydate(c("1908", "1908-10", "1908", "1908", "1998",
                      "XXXX-01-01", "XXXX-01-01", "1998-10", "1998"))
  expect_equal(as_messydate(unspecified), b)
})

test_that("resequence argument works properly", {
  expect_equal(as_messydate(c("121008", "20121008"), resequence = "ymd"),
               as_messydate(c("12-10-08", "2012-10-08")))
  expect_equal(as_messydate(c("081012", "08102012", "08-10-12"), resequence = "dmy"),
               as_messydate(c("12-10-08", "2012-10-08", "12-10-08")))
  expect_equal(as_messydate(c("03312022", "043097"), resequence = "mdy"),
               as_messydate(c("2022-03-31", "97-04-30")))
  expect_equal(as_messydate("201212", resequence = "ym"),
               as_messydate("2012-12"))
  expect_equal(as_messydate("201212", resequence = "my"),
               as_messydate("1212-20"))
})

test_that("dates are properly extracted from text", {
  expect_equal(as_messydate(c("This function was created on the 29 of September 2021",
                              "Tomorrow is 13-10-2021",
                              "Second of February, two thousand and twenty-two")),
               as_messydate(c("2021-09-29", "2021-10-13", "2022-02-02")))
  expect_equal(as_messydate(c("signed on this thirtieth day of October one thousand nine hundred and forty-seven",
                            "signed on one thousand nine hundred and forty-seven, on the month of October, the thirtieth day",
                            "signed on this twenty-first day of October one thousand nine hundred and forty-seven",
                            "twenty second day of November 2022")),
               as_messydate(c("1947-10-30", "1947-10-30", "1947-10-21", "22-11-2022")))
})

month_dates <- c("Sep 13, 1988", "Jul 11, 2003", "May 28, 1996", "Oct 2, 2009",
                 "1990, Apr 20", "2006, 22 Nov", "1996, Oct 25", "1997, 2 Dec")
dmy <- c("1988-09-13", "2003-07-11", "1996-05-28", "2009-10-02",
         "1990-04-20", "2006-11-22", "1996-10-25", "02-12-1997")

test_that("conversion from MDY dates with written month works properly", {
  expect_equal(as_messydate(month_dates), as_messydate(dmy))
})

test_that("list conversion works properly", {
  expect_equal(as_messydate(list(c("2012-06-01", "2012-06-02", "2012-06-03"))),
               list(as_messydate("2012-06-01..2012-06-03")))
  expect_equal(as_messydate(list(c(as_messydate("2001-01-01"),
                                   as_messydate("2001-01-02..2001-01-04")))),
               list(as_messydate("2001-01-01..2001-01-04")))
})
