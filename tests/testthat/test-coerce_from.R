messy <- as_messydate("2010-10-10..2010-10-20")
ddate <- as.Date("2010-10-10")
mdatey <- as_messydate("2010-10-10")
# negative <- min(as_messydate("1000 BC"))

test_that("Coercion from other date classes into messydt works", {
  # expect_equal(as.character(as.Date(as_messydate("1000 BC"), max)), "-1000-12-31")
  expect_equal(as.Date(messy, FUN = min), ddate)
  # expect_equal(as.Date(mdatey, FUN = median), ddate)
  expect_equal(as.Date(mdatey, FUN = random), ddate)
  # expect_equal(as.character(as.Date(as_messydate("1000 BC"), min)), min(negative))
})

test_that("Coercion to POSIX works", {
  expect_equal(as.POSIXct(messy, FUN = max), as.POSIXct("2010-10-20 CEST"))
  # expect_equal(as.POSIXlt(messy, FUN = mean), as.POSIXlt("2010-10-15 CEST"))
})

# neg_dates <- as_messydate(c("-27", "-14"))
# test_that("Coercion from other types of negative dates work", {
#   expect_equal(min(neg_dates), c("-0027-01-01", "-0014-01-01"))
#   expect_equal(max(neg_dates), c("-0027-12-31", "-0014-12-31"))
#   expect_equal(mean(neg_dates), c("-0027-07-02", "-0014-07-02"))
# })

# expect_error(as.POSIXct(as_messydate("-2012"), min))
# expect_error(as.POSIXlt(as_messydate("-2012"), min))
