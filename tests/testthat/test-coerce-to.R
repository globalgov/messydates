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
  expect_equal(as_messydate(c("This function was created on the 29 September 2021",
                              "Tomorrow is 13-10-2021"), from_text = TRUE),
               as_messydate(c("29-9-2021", "13-10-2021")))
  expect_equal(as_messydate("signed on this thirtieth day of October one thousand nine hundred and forty-seven",
                            from_text = TRUE),
               as_messydate("1947-10-30"))
  expect_equal(as_messydate("signed on this thirtieth day of October one thousand nine hundred and forty-seven",
                            from_text = TRUE),
               as_messydate("30-10-1947"))
  expect_equal(as_messydate("signed on this twenty-first day of October one thousand nine hundred and forty-seven",
                            from_text = TRUE),
               as_messydate("1947-10-21"))
})
