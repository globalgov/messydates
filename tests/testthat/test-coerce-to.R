test_that("Coercion from other date classes into messydt works", {
  date <- as.Date("2010-10-10")
  POSIXct <- as.POSIXct("2010-10-10")
  POSIXlt <- as.POSIXlt("2010-10-10")
  character <- "2010-10-10"
  character2 <- "AD2010-10-10"
  character3 <- "{BC2010-10-10,BC2010-10-11,BC2010-10-12}"
  messy <- as_messydate("2010-10-10")
  messyneg <- as_messydate("{-2010-10-10,-2010-10-11,-2010-10-12}")
  expect_equal(as_messydate(date), messy)
  expect_equal(as_messydate(POSIXct), messy)
  expect_equal(as_messydate(POSIXlt), messy)
  expect_equal(as_messydate(character), messy)
  expect_equal(as_messydate(character2), messy)
  expect_equal(as_messydate(character3), messyneg)
})
