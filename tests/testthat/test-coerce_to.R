test_that("Coercion from other date classes into messydt works", {
  date <- as.Date("2010-10-10")
  POSIXct <- as.POSIXct("2010-10-10", tz = "UTC")
  POSIXlt <- as.POSIXlt("2010-10-10", tz = "UTC")
  character <- "2010-10-10"
  character2 <- "AD2010-10-10"
  character3 <- "{BC2010-10-10,BC2010-10-11,BC2010-10-12}"
  dmy_text <- "10 October 2010"
  mdy_text <- "October 10, 2010"
  messy <- as_messydate("2010-10-10")
  # "BC2010" is historical 2010 BCE, i.e. astronomical year -2009 (year zero
  # exists in ISO 8601-2), so the prose set below resolves to -2009-...
  messyneg <- as_messydate("{-2009-10-10,-2009-10-11,-2009-10-12}")
  expect_equal(as_messydate(date), messy)
  expect_equal(as_messydate(POSIXct), messy)
  expect_equal(as_messydate(POSIXlt), messy)
  expect_equal(as_messydate(character), messy)
  expect_equal(as_messydate(character2), messy)
  expect_equal(as_messydate(character3), messyneg)
  expect_equal(as_messydate(dmy_text), messy)
  expect_equal(as_messydate(mdy_text), messy)
  expect_equal(mdate(date), messy)
  expect_equal(mdate(POSIXct), messy)
  expect_equal(mdate(POSIXlt), messy)
  expect_equal(mdate(character), messy)
  expect_equal(mdate(character2), messy)
  expect_equal(mdate(character3), messyneg)
  expect_equal(mdate(dmy_text), messy)
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
  # "201212" read as month-year gives month 20, which does not exist
  expect_error(as_messydate("201212", resequence = "my"),
               "not between 01 and 12")
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
                 "1990, Apr 20", "2006, 22 Nov", "1996, Oct 25", "1997, 2 Dec",
                 "Jan-1990")
dmy <- c("1988-09-13", "2003-07-11", "1996-05-28", "2009-10-02",
         "1990-04-20", "2006-11-22", "1996-10-25", "02-12-1997", "1990-01")

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

test_that("zero padding is correctly added conversion works properly", {
  expect_equal(as_messydate(c("193-3", "193-3..193-5", "193-3, 193-4")),
               as_messydate(c("0193-03-XX", "0193-03-XX..0193-05", "{0193-03-XX,0193-04-XX}")))
})

test_that("an era marker applies to every date it governs", {
  # A marker written once at the end governs each bound of a range or set,
  # including an open range where no year precedes the marker.
  expect_equal(unclass(as_messydate("200..100 BC")), "-0199..-0099")
  expect_equal(unclass(as_messydate("..200 BC")), "..-199")
  expect_equal(unclass(as_messydate("200 BC..")), "-199..")
  expect_equal(unclass(as_messydate("44 BC, 33 BC")), "{-0043,-0032}")
  expect_equal(unclass(as_messydate("44, 33 BC")), "{-0043,-0032}")
  # A marker written before a date governs that date, not the one before it.
  # (Four-digit years, since zero padding is only applied to the first
  # element of a set, whatever the era.)
  expect_equal(unclass(as_messydate("{BC1044-03-15,BC1033-01-01}")),
               "{-1043-03-15,-1032-01-01}")
  # Each bound may carry its own era.
  expect_equal(unclass(as_messydate("200 BC..100 AD")), "-0199..0100")
  # Months and days are not mistaken for years, and a signed astronomical
  # year is left alone.
  expect_equal(unclass(as_messydate("44-03-15 BC")), "-0043-03-15")
  expect_equal(unclass(as_messydate("-0044..-0033")), "-0044..-0033")
  # An era marker survives an approximate or uncertain prose qualifier.
  expect_equal(unclass(as_messydate("circa 200 BC")), "~-199")
  expect_equal(unclass(as_messydate("possibly 44-03-15 BC")), "-0043-03-15?")
})

test_that("prose dates parse the same alone as in a vector", {
  # Regression test: the month-first reorder used to inspect only the first
  # element of the vector and apply only when the vector had length one, so
  # "July 4 1976" lost its day whenever it had a neighbour.
  expect_equal(unclass(as_messydate("July 4 1976")), "1976-07-04")
  expect_equal(unclass(as_messydate(c("July 4 1976", "May 1 1980"))),
               c("1976-07-04", "1980-05-01"))
  expect_equal(unclass(as_messydate(c("2019-01-01", "July 4 1976"))),
               c("2019-01-01", "1976-07-04"))
})

test_that("factors and unsupported classes are handled", {
  expect_equal(unclass(as_messydate(factor(c("2019-01-01", "2019-02")))),
               c("2019-01-01", "2019-02"))
  expect_error(as_messydate(TRUE), "cannot be coerced to 'mdate'")
})

test_that("unrepresentable ISO 8601-2 formats are rejected", {
  # Week dates are read (see the week tests below); ordinal dates and season
  # codes are not, being too easily read as a mistake for an ordinary date.
  expect_error(as_messydate("2019-123"), "ordinal dates")
  expect_error(as_messydate("2019-123"), "123rd day of 2019")
  expect_error(as_messydate("2019-21"), "season codes")
  expect_error(as_messydate("2019-21"), "spring 2019")
  expect_error(as_messydate("1234S3"), "significant digits")
  expect_error(as_messydate("Y17E7"), "extended years")
  # Durations themselves are supported, as date ranges; only the notation is not.
  expect_error(as_messydate("P1Y2M"), "ISO duration notation")
  expect_error(as_messydate("P1Y2M"), "make_messyduration")
  expect_error(as_messydate("R5/2019-01-01/P1Y"), "repeating intervals")
  # Prose beginning with the same letters is not mistaken for these formats.
  expect_equal(unclass(as_messydate("prior to 2019-01-01")), "..2019-01-01")
})

test_that("impossible date components are rejected", {
  expect_error(as_messydate("2019-02-30"), "only 28 days")
  expect_error(as_messydate("2019-06-31"), "only 30 days")
  expect_error(as_messydate("2019-13-45"), "not between 01 and 12")
  expect_error(as_messydate("2019-01-01 25:00"), "greater than 23")
  # Leap years, unspecified months, and ranges remain valid.
  expect_equal(unclass(as_messydate("2020-02-29")), "2020-02-29")
  expect_equal(unclass(as_messydate("2008-XX-31")), "2008-XX-31")
  expect_equal(unclass(as_messydate("2019-01-01..2019-02-28")),
               "2019-01-01..2019-02-28")
})

test_that("unparseable text warns rather than failing silently", {
  expect_warning(as_messydate("not a date"), "could not be parsed")
  expect_equal(unclass(suppressWarnings(as_messydate(c("2019-01-01", "nope")))),
               c("2019-01-01", NA))
  expect_silent(as_messydate(c("2019-01-01", NA)))
})

test_that("md_problems() reports why elements fail", {
  p <- md_problems(c("2019-01-01", "2019-02-30", "2019-123", "not a date"))
  expect_equal(p$index, c(2L, 3L, 4L))
  expect_match(p$reason[1], "only 28 days")
  expect_match(p$reason[2], "ordinal dates")
  expect_match(p$reason[3], "could not be parsed")
  # A clean vector produces no rows.
  expect_equal(nrow(md_problems(c("2019-01-01", "2019-01"))), 0L)
  expect_equal(nrow(md_problems(character(0))), 0L)
})

test_that("a day of the year is converted to its calendar date", {
  expect_identical(as_messydate("103rd day of 2026"),
                   as_messydate("2026-04-13"))
  expect_identical(as_messydate("day 103 of 2026"),
                   as_messydate("2026-04-13"))
  # A leap year shifts every day after the 59th.
  expect_identical(as_messydate("60th day of 2024"),
                   as_messydate("2024-02-29"))
  expect_identical(as_messydate("60th day of 2026"),
                   as_messydate("2026-03-01"))
  # The ends of the year.
  expect_identical(as_messydate("day 1 of 2026"), as_messydate("2026-01-01"))
  expect_identical(as_messydate("365th day of 2026"),
                   as_messydate("2026-12-31"))
  expect_identical(as_messydate("366th day of 2024"),
                   as_messydate("2024-12-31"))
  # A day the year does not have cannot be parsed.
  expect_warning(as_messydate("400th day of 2026"), "could not be parsed")
  expect_warning(as_messydate("366th day of 2026"), "could not be parsed")
})

test_that("a week of the year is converted to a range of seven days", {
  expect_identical(as_messydate("5th week of 2026"),
                   as_messydate("2026-01-26..2026-02-01"))
  expect_identical(as_messydate("week 5 of 2026"),
                   as_messydate("2026-01-26..2026-02-01"))
  # ISO weeks belong to the year holding their Thursday, so week 1 of 2026
  # starts in December 2025.
  expect_identical(as_messydate("1st week of 2026"),
                   as_messydate("2025-12-29..2026-01-04"))
  # A year has 53 ISO weeks when it starts on a Thursday, or when it is a leap
  # year starting on a Wednesday. 2020 and 2026 do, 2025 does not.
  expect_identical(as_messydate("53rd week of 2020"),
                   as_messydate("2020-12-28..2021-01-03"))
  expect_identical(as_messydate("53rd week of 2026"),
                   as_messydate("2026-12-28..2027-01-03"))
  expect_warning(as_messydate("53rd week of 2025"), "could not be parsed")
  expect_warning(as_messydate("0th week of 2026"), "could not be parsed")
})

test_that("ISO week dates are read but never written", {
  expect_identical(as_messydate("2026-W05"), as_messydate("5th week of 2026"))
  expect_identical(as_messydate("2019-W12"),
                   as_messydate("2019-03-18..2019-03-24"))
  # A weekday within the week, 1 being Monday.
  expect_identical(as_messydate("2026-W05-3"), as_messydate("2026-01-28"))
  expect_identical(as_messydate("2026-W05-7"), as_messydate("2026-02-01"))
})

test_that("weeks and days of the year combine with other prose", {
  expect_equal(unclass(as_messydate("before the 5th week of 2026")),
               "..2026-01-26")
  expect_equal(unclass(as_messydate("after the 5th week of 2026")),
               "2026-02-01..")
  expect_equal(unclass(as_messydate("around the 5th week of 2026")),
               "2026-01-26~..2026-02-01~")
  expect_equal(unclass(as_messydate("possibly the 103rd day of 2026")),
               "2026-04-13?")
})

test_that("ordinary dates and durations are not read as weeks or days", {
  expect_equal(unclass(as_messydate("2026-01-05")), "2026-01-05")
  expect_equal(unclass(as_messydate("5 January 2026")), "2026-01-05")
  # The year has to be introduced by "of" or "in", so a duration is left alone.
  expect_warning(as_messydate("5 weeks"), "could not be parsed")
  expect_warning(as_messydate("103 days"), "could not be parsed")
})
