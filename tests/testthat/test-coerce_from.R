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
  expect_identical(as_messydate("from the 13th to the 15th of Feb, 1977"), as_messydate("{1977-02-13..1977-02-15}"))
  expect_identical(as_messydate("from February 13 to 15, 1977"), as_messydate("{1977-02-13..1977-02-15}"))
  expect_identical(as_messydate("it happened on the 13th or the 15th of Feb, 1977"), as_messydate("{1977-02-13,1977-02-15}"))
  expect_identical(as_messydate("things happened on 13th and 15th of Feb, 1977"), as_messydate(c("1977-02-13","1977-02-15")))
  expect_identical(as_messydate("13th Feb, 1977, Feb 15 1977, 1910"), as_messydate(c("1977-02-13","1977-02-15","1910")))
  expect_identical(as_messydate("it happened possibly about 1910"), as_messydate("%1910"))
  expect_identical(as_messydate("it happened in the 19th century"), as_messydate("18XX"))
  expect_identical(as_messydate("it happened in the 1910s"), as_messydate("191X"))
  expect_identical(as_messydate("it happened before 1910"), as_messydate("..1910"))
  expect_identical(as_messydate("it happened after 1910"), as_messydate("1910.."))
})

test_that("approximate/uncertain qualifiers keep the full date", {
  # Regression: qualifiers used to collapse a fully specified date to its year
  # (e.g. "~2024"), or misread the month name embedded in the qualifier word
  # ("may" inside "maybe" gave "2024-?05").
  expect_identical(as_messydate("Approximately 2024-01-22"), as_messydate("2024-01-22~"))
  expect_identical(as_messydate("Maybe 2024-02-02"), as_messydate("2024-02-02?"))
  expect_identical(as_messydate("around 2024-04-04"), as_messydate("2024-04-04~"))
  expect_identical(as_messydate("perhaps 2024-03-03"), as_messydate("2024-03-03?"))
  expect_identical(as_messydate("roughly 1999-12-31"), as_messydate("1999-12-31~"))
  # Month-precision dates keep their month.
  expect_identical(as_messydate("circa 2012-03"), as_messydate("2012-03~"))
  expect_identical(as_messydate("maybe March 1910"), as_messydate("1910-03?"))
  # Year-precision qualifiers are unchanged.
  expect_identical(as_messydate("circa 2012"), as_messydate("~2012"))
  expect_identical(as_messydate("possibly 1850"), as_messydate("?1850"))
})

test_that("Roman date parsing works correctly", {
  expect_identical(as_messydate("MDCCLXXVI"), as_messydate("1776"))
  expect_identical(as_messydate("the Kalends of March, 44 BC"), as_messydate("-0044-03-01"))
  expect_identical(as_messydate("the Nones of February, 44 BC"), as_messydate("-0044-02-05"))
  expect_identical(as_messydate("the Nones of March, 44 BC"), as_messydate("-0044-03-07"))
  expect_identical(as_messydate("the Ides of February, 44 BC"), as_messydate("-0044-02-13"))
  expect_identical(as_messydate("the Ides of March, 44 BC"), as_messydate("-0044-03-15"))
})

# Seasons and relative parts of a year are interpreted as month ranges. This
# behaviour is intentionally undocumented (not part of the advertised API), as
# the conventions vary; the tests pin the current behaviour.
test_that("seasons and relative periods parse to month ranges", {
  expect_identical(as_messydate("Spring 1918"), as_messydate("1918-03..1918-05"))
  expect_identical(as_messydate("Summer 1918"), as_messydate("1918-06..1918-08"))
  expect_identical(as_messydate("Autumn 1918"), as_messydate("1918-09..1918-11"))
  expect_identical(as_messydate("Fall 1918"), as_messydate("1918-09..1918-11"))
  expect_identical(as_messydate("Winter 1918"), as_messydate("1918-12..1919-02"))
  expect_identical(as_messydate("early 1918"), as_messydate("1918-01..1918-04"))
  expect_identical(as_messydate("mid 1918"), as_messydate("1918-05..1918-08"))
  expect_identical(as_messydate("late 1918"), as_messydate("1918-09..1918-12"))
})

test_that("as.POSIXct/as.POSIXlt error for dates before the common era", {
  expect_error(as.POSIXct(as_messydate("-2012"), FUN = min), "as.Date")
  expect_error(as.POSIXlt(as_messydate("-2012"), FUN = min), "as.Date")
})

test_that("as.double.mdate converts to days since 1970-01-01, including BCE", {
  expect_equal(as.double(as_messydate("2012-01-01")),
               as.double(as.Date("2012-01-01")))
  expect_equal(as.double(as_messydate("-1000-01-01")),
               as.double(lubridate::as_date(lubridate::ymd("0000-01-01") -
                                              lubridate::years(1000))))
})
