messy <- as_messydate("2010-10-10..2010-10-20")
ddate <- as.Date("2010-10-10")
mdatey <- as_messydate("2010-10-10")
# negative <- min(as_messydate("1000 BC"))

test_that("Coercion from other date classes into messydt works", {
  # expect_equal(as.character(as.Date(as_messydate("1000 BC"), max)), "-1000-12-31")
  expect_equal(as.Date(messy, FUN = vmin), ddate)
  # expect_equal(as.Date(mdatey, FUN = median), ddate)
  expect_equal(as.Date(mdatey, FUN = random), ddate)
  # expect_equal(as.character(as.Date(as_messydate("1000 BC"), min)), min(negative))
})

test_that("Coercion to POSIX works", {
  expect_equal(as.POSIXct(messy, FUN = vmax), as.POSIXct("2010-10-20", tz = "UTC"))
  # expect_equal(as.POSIXlt(messy, FUN = mean), as.POSIXlt("2010-10-15 CEST"))
})

# neg_dates <- as_messydate(c("-27", "-14"))
# test_that("Coercion from other types of negative dates work", {
#   expect_equal(min(neg_dates), c("-0027-01-01", "-0014-01-01"))
#   expect_equal(max(neg_dates), c("-0027-12-31", "-0014-12-31"))
#   expect_equal(mean(neg_dates), c("-0027-07-02", "-0014-07-02"))
# })

test_that("Text parsing works correctly", {
  expect_identical(as_messydate("Fourth of July 1976"), as_messydate("1976-07-04"))
  expect_identical(as_messydate("Fourth of July 19766"), as_messydate("1976-07-04"))
  expect_identical(as_messydate("4th July 1976"), as_messydate("1976-07-04"))
  expect_identical(as_messydate("July Fourth 1976"), as_messydate("1976-07-04"))
  expect_identical(as_messydate("July 4th 1976"), as_messydate("1976-07-04"))
  expect_identical(as_messydate("4th day of July, 1976"), as_messydate("1976-07-04"))
  expect_identical(as_messydate("Last day of July, 1976"), as_messydate("1976-07-31"))
  expect_identical(as_messydate("February 2004"), as_messydate("2004-02"))
  expect_identical(as_messydate("signed on the last day of February 2004"), as_messydate("2004-02-29"))
  expect_identical(as_messydate("it happened around the 13th of Feb in 1977"), as_messydate("1977-02-~13"))
  expect_identical(as_messydate("it happened between the 13th and 15th of Feb, 1977"), as_messydate("1977-02-13..1977-02-15"))
  expect_identical(as_messydate("it happened on the 13th or the 15th of Feb, 1977"), as_messydate("{1977-02-13,1977-02-15}"))
  expect_identical(as_messydate("things happened on 13th and 15th of Feb, 1977"), as_messydate(c("1977-02-13","1977-02-15")))
})

test_that("Roman date parsing works correctly", {
  expect_identical(as_messydate("MDCCLXXVI"), as_messydate("1776"))
  expect_identical(as_messydate("the Kalends of March, 44 BC"), as_messydate("-0044-03-01"))
  expect_identical(as_messydate("the Nones of February, 44 BC"), as_messydate("-0044-02-05"))
  expect_identical(as_messydate("the Nones of March, 44 BC"), as_messydate("-0044-03-07"))
  expect_identical(as_messydate("the Ides of February, 44 BC"), as_messydate("-0044-02-13"))
  expect_identical(as_messydate("the Ides of March, 44 BC"), as_messydate("-0044-03-15"))
})

# expect_error(as.POSIXct(as_messydate("-2012"), min))
# expect_error(as.POSIXlt(as_messydate("-2012"), min))
