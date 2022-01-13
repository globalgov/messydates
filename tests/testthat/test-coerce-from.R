test_that("Coercion from other date classes into messydt works", {
  date <- as.Date("2010-10-10")
  POSIXct <- as.POSIXct("2010-10-20 CEST")
  POSIXlt <- as.POSIXlt("2010-10-15 CEST")
  character <- "2010-10-10"
  messy <- as_messydate("2010-10-10..2010-10-20")
  negative <- as_messydate("28 BC")
  expect_equal(as.character(as.Date(as_messydate("1000 BC"), max)), "-1000-12-31")
  expect_equal(as.Date(messy, min), date)
  expect_equal(as.POSIXct(messy, max), POSIXct)
  expect_equal(as.POSIXlt(messy, median), POSIXlt)
  expect_equal(as.character(as.Date(negative, min)), "-028-01-01")
})
