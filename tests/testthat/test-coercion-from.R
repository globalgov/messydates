test_that("Coercion from other date classes into messydt works", {
  date <- as.Date("2010-10-10")
  POSIXct <- as.POSIXct("2010-10-10")
  POSIXlt <- as.POSIXlt("2010-10-10")
  character <- "2010-10-10"
  messy <- as_messydate("2010-10-10")
  expect_equal(as_messydate.Date(date), messy)
  expect_equal(as_messydate.POSIXct(POSIXct), messy)
  expect_equal(as_messydate.POSIXlt(POSIXlt), messy)
  expect_equal(as_messydate.character(character), messy)
})
