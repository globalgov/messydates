test_dates <- c(regular = "2010-01-01",
                text = "Second of February, two thousand and twenty-two",
                range = "2014-01-01..2014-01-03",
                # approxyr = "~1999",
                # approxmt = "1999-10~",
                approxdy = "1999-10-~11",
                unspec = "2008-XX-03",
                set = "{2012-01-01,2012-01-12}",
                neg = "20 BC",
                # unspecrange = "2010..2010-12",
                # negincomrange = "200 BC:199 BC",
                negincomset = "{-200, -199}")
out <- expand(test_dates)
# regular_date <- as.Date("2010-01-01")
# text_date <- "Second of February, two thousand and twenty-two"
# range <- as_messydate("2014-01-01..2014-01-03")
# approximate <- as_messydate(c("~1999", "1999-10~", "1999-10-~11"))
# unspecified <- as_messydate("2008-XX-03")
# set <- as_messydate("{2012-01-01,2012-01-12}")
# negative <- as_messydate("20 BC")
# unspecified_range <- as_messydate("2010..2010-12")
# negative_incomplete_range <- as_messydate("200 BC:199 BC")
# negative_incomplete_set <- as_messydate("{-200, -199}")


test_that("Expand dates lengths are correct", {
  expect_equal(length(out), length(test_dates))
  expect_equal(vapply(out, length, FUN.VALUE = numeric(1)),
               c(1,1,3,#365,31,
                 1,12,2,366,730))
})

# test_that("Expand dates works properly for date ranges and unspecified dates", {
#   expect_equal(expand(regular_date), expand(as_messydate(regular_date)))
#   expect_length(expand(text_date), 1)
#   expect_equal(as.character(expand(range)),
#                "c(\"2014-01-01\", \"2014-01-02\", \"2014-01-03\")")
#   expect_equal(as.character(expand(approximate)[[1]][1]), "1999-01-01")
#   expect_equal(as.character(expand(approximate, approx_range = 3)[[2]][1]), "1996-07-01")
#   expect_equal(as.character(expand(approximate, approx_range = 3)[[3]][1]), "1999-10-08")
#   expect_equal(as.character(expand(unspecified)[[1]][1]), "2008-01-03")
#   expect_equal(as.character(expand(set)[[1]][1]), "2012-01-01")
#   expect_length(expand(range), 1)
#   expect_length(expand(unspecified), 1)
#   expect_equal(lengths(expand(negative)), 366)
#   expect_equal(as.character(expand(unspecified_range)[[1]][1]), "2010-01-01")
#   expect_equal(lengths(expand(unspecified_range)), 365)
#   expect_equal(lengths(expand(negative_incomplete_range)), 730)
#   expect_equal(lengths(expand(negative_incomplete_set)), 730)
# })

# ly <- as_messydate("~2000-01-01")
# lym <- as_messydate("2000-~02-01")
# test_that("Expand approximate works properly for leap years", {
#   expect_equal(lengths(expand(ly, approx_range = 1)), 732)
#   expect_equal(lengths(expand(lym, approx_range = 1)), 61)
# })
