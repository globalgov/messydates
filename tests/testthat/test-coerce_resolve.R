# "1004-02 BC" is historical Feb of 1004 BCE, i.e. astronomical -1003-02
# (year zero exists in ISO 8601-2). Astronomical year -1003 is not a leap
# year, so its February has 28 days.
test_dates <- c(range = as_messydate("2014-01-01..2014-01-05"),
                unspec = as_messydate("1999"),
                neg = as_messydate("1004-02 BC"))
# test_dates <- lapply(test_dates, as_messydate)

test_that("Min resolving works properly", {
  expect_equal(unname(as.character(vmin(test_dates))),
               c("2014-01-01","1999-01-01","-1003-02-01"))
})

test_that("Max resolving works properly", {
  expect_equal(unname(as.character(vmax(test_dates))),
               c("2014-01-05","1999-12-31","-1003-02-28"))
})

test_that("Vectorised median resolving works properly", {
  expect_equal(unname(as.character(vmedian(test_dates))),
               c("2014-01-03","1999-07-02","-1003-02-15"))
})

test_that("Vectorised mean resolving works properly for CE dates", {
  # Averaging BCE dates is a known limitation (see ?coerce_tendency), so only
  # the two CE elements are checked here.
  expect_equal(unname(as.character(vmean(test_dates)[1:2])), c("2014-01-03","1999-07-02"))
})

test_that("Vectorised modal resolving works properly", {
  expect_equal(unname(as.character(vmodal(test_dates))),
               c("2014-01-01","1999-01-01","-1003-02-01"))
})

test_that("median() and mean() summarise a whole mdate vector to one value", {
  # Unlike vmedian()/vmean(), these expand and combine *all* elements first.
  single <- as_messydate("2014-01-01..2014-01-05")
  expect_equal(as.character(median(single)), "2014-01-03")
  expect_equal(as.character(mean(single)), "2014-01-03")
})

test_that("median() and mean() average an even number of expanded dates", {
  # Regression test: base R's median() cannot average two character dates,
  # so an even-length expansion (e.g. a 4-day range) used to return NA.
  even <- as_messydate("2001-01-01..2001-01-04")
  expect_equal(as.character(median(even)), "2001-01-02")
  expect_equal(as.character(mean(even)), "2001-01-02")
})

test_that("median() and mean() average precise date-times", {
  dt <- as_messydate(c("2012-06-01T09:00", "2012-06-01T17:00"))
  expect_equal(as.character(median(dt)), "2012-06-01 13:00:00")
  expect_equal(as.character(mean(dt)), "2012-06-01 13:00:00")
})

# test_that("Resolve dates works properly for date ranges", {
#   # range2 <- as_messydate("2014-01-01..2014-01-30")
#   # expect_equal(as.character(min(range)), "2014-01-01")
#   # expect_equal(as.character(max(range)), "2014-01-31")
#   # expect_equal(as.character(median(range)), "2014-01-16")
#   # expect_equal(as.character(median(range2)), "2014-01-16")
#   # expect_equal(as.character(mean(range)), "2014-01-16")
#   # expect_equal(as.character(modal(range)), "2014-01-01")
#   expect_length(random(range), 1)
# })

# test_that("Resolve dates works properly for unspecified dates", {
#   unspecified <- as_messydate("1999")
#   # expect_equal(as.character(min(unspecified)), "1999-01-01")
#   # expect_equal(as.character(max(unspecified)), "1999-12-31")
#   # expect_equal(as.character(median(unspecified)), "1999-07-02")
#   expect_equal(as.character(mean(unspecified)), "1999-07-02")
#   expect_equal(as.character(modal(unspecified)), "1999-01-01")
#   expect_length(random(unspecified), 1)
# })

# test_that("Resolve dates works properly for negative dates", {
#   negative <- as_messydate("1000 BC")
#   expect_equal(as.character(min(negative)), "-1000-01-01")
#   expect_equal(as.character(as.Date(negative, min)), "-1000-01-01")
#   expect_equal(as.character(max(negative)), "-1000-12-31")
#   expect_equal(as.character(as.Date(negative, max)), "-1000-12-31")
#   expect_equal(as.character(median(negative)), "-1000-07-02")
#   expect_equal(as.character(as.Date(negative, median)), "-1000-07-02")
#   expect_equal(as.character(mean(negative)), "-1000-07-02")
#   expect_equal(as.character(as.Date(negative, mean)), "-1000-07-02")
#   expect_equal(as.character(modal(negative)), "-1000-01-01")
#   expect_equal(as.character(as.Date(negative, modal)), "-1000-01-01")
#   # expect_length(random(negative), 1)
# })

test_that("as_mdate adds zero padding when appropriate", {
  # expect_equal(as_messydate(min(as_messydate("209-12-31"))),
  #                           as_messydate("0209-12-31"))
  # expect_equal(as_messydate(max(as_messydate("-29-12-31"))),
  #                           as_messydate("-0029-12-31"))
  expect_equal(as_messydate(c("-29-12-31", "193-02-02", "2010-10-10")),
               as_messydate(c("-0029-12-31", "0193-02-02", "2010-10-10")))
})

test_that("'[]' and '{}' sets resolve alike", {
  onesie <- as_messydate("[2001-01-01,2001-02-02]")
  allof <- as_messydate("{2001-01-01,2001-02-02}")
  expect_equal(vmin(onesie), vmin(allof))
  expect_equal(vmax(onesie), vmax(allof))
  expect_equal(vmean(onesie), vmean(allof))
  expect_equal(vmedian(onesie), vmedian(allof))
  expect_true(vrandom(onesie) %in% c("2001-01-01", "2001-02-02"))
})

test_that("vrandom() fast paths draw uniformly within full-date ranges", {
  x <- as_messydate(c("2001-01-10..2001-01-20", "2001-03-05",
                      "2001-02", "{2001-01-01,2001-02-02}",
                      "2001-12-30..2002-01-02"))
  set.seed(1)
  draws <- replicate(50, as.character(vrandom(x)))
  # every draw lies inside its value's compatible dates, and order is kept
  expect_true(all(as.Date(draws[1, ]) >= as.Date("2001-01-10") &
                    as.Date(draws[1, ]) <= as.Date("2001-01-20")))
  expect_true(all(substr(draws[3, ], 1, 7) == "2001-02"))
  expect_true(all(draws[4, ] %in% c("2001-01-01", "2001-02-02")))
  expect_true(all(draws[5, ] %in% c("2001-12-30", "2001-12-31",
                                    "2002-01-01", "2002-01-02")))
  # both ends of a range can be drawn
  expect_true(all(c("2001-01-10", "2001-01-20") %in% draws[1, ]))
  # the draws are uniform over the eleven days of the range
  big <- vrandom(as_messydate(rep("2001-01-10..2001-01-20", 2200)))
  expect_gt(stats::chisq.test(table(big))$p.value, 0.001)
})

test_that("vmean() and vmedian() fast paths match the expanded computation", {
  set.seed(7)
  lo <- as.Date("1995-01-01") + sample(0:9000, 150, TRUE)
  hi <- lo + sample(0:399, 150, TRUE)
  x <- as_messydate(c(paste0(lo, "..", hi), "2001-02", "2004-02", "1900-02",
                      "2001-12", "2001", "2000", "1900",
                      "{2001-01-01,2001-02-02}", "2012-06-02",
                      "2000-12-30..2001-01-02"))
  expanded_mean <- vapply(compatible_dates(x), .mean_value, character(1))
  expanded_median <- vapply(compatible_dates(x), function(y)
    .median_value(.order_messy(y)), character(1))
  expect_identical(as.character(vmean(x)), expanded_mean)
  expect_identical(as.character(vmedian(x)), expanded_median)
})

test_that("vmodal() fast path matches the expanded computation", {
  x <- as_messydate(c("2001-02", "2001", "2000-12-30..2001-01-02",
                      "{2001-03-01,2001-02-02}", "2001-XX-15"))
  expect_identical(as.character(vmodal(x)),
                   vapply(compatible_dates(x), .getmode, character(1)))
})

test_that("resolved values in the canonical notation skip the parser unchanged", {
  x <- c("2012-01-01", "-0044-02-29", "2012-01-01 14:30", "2012-01-01 14:30:05.5",
         "2012-01-01 14:30:00Z", "2012-01-01 14:30:00-05:30", NA)
  expect_true(all(.is_canonical_resolved(x[-7])))
  expect_identical(as.character(.as_resolved(x)), as.character(mdate(x)))
  # impossible values still go to the parser, which rejects them
  expect_false(any(.is_canonical_resolved(c("2013-02-29", "2012-01-01 24:00"))))
  expect_error(.as_resolved("2013-02-29"), "Impossible")
})
