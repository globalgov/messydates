test_that("Coercion from other date classes into messydt works", {
  date <- as.Date("2010-10-10")
  POSIXct <- as.POSIXct("2010-10-10")
  POSIXlt <- as.POSIXlt("2010-10-10")
  character <- "2010-10-10"
  messy <- as_messydate("2010-10-10")
  expect_equal(as.Date.messydt(messy), date)
  expect_equal(as.POSIXct.messydt(messy), POSIXct)
  expect_equal(as.POSIXlt.messydt(messy), POSIXlt)
})
