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
