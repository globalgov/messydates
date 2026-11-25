
test_that("min() finds the earliest value in any position", {
  x <- as_messydate(c("2019-03-01", "2012-06", "2012-06-15", "2019-03-01"))
  expect_equal(as.character(min(x)), "2012-06-01")
  expect_equal(as.character(max(x)), "2019-03-01")
})

test_that("the extrema remove % as well as ~ and ?", {
  x <- as_messydate(c("2019-03-%01", "%2012-06", "2012-06~-%15", "2019-03-01%"))
  expect_equal(as.character(vmin(x)),
               c("2019-03-01", "2012-06-01", "2012-06-15", "2019-03-01"))
  expect_equal(as.character(vmax(x)),
               c("2019-03-01", "2012-06-30", "2012-06-15", "2019-03-01"))
  expect_equal(as.character(min(x)), "2012-06-01")
  expect_equal(as.character(max(x)), "2019-03-01")
})

test_that("vmin and vmax widen approximate dates by approx_range", {
  x <- as_messydate(c("2001-05-16~", "2001-03-~01..2001-04-15", "2001-05-16",
                      "2001-05", NA))
  expect_equal(as.character(vmin(x, approx_range = 3)),
               c("2001-05-13", "2001-02-26", "2001-05-16", "2001-05-01", NA))
  expect_equal(as.character(vmax(x, approx_range = 3)),
               c("2001-05-19", "2001-04-15", "2001-05-16", "2001-05-31", NA))
  # approx_range = 0, the default, ignores the marks as before
  expect_equal(vmin(x, approx_range = 0), vmin(x))
  expect_equal(as.character(vmin(x))[1], "2001-05-16")
  # the bounds are those of compatible_dates() and of vrandom()
  pd <- compatible_dates(x[1:4], approx_range = 3)
  expect_equal(as.character(vmin(x[1:4], approx_range = 3)), vapply(pd, min, ""))
  expect_equal(as.character(vmax(x[1:4], approx_range = 3)), vapply(pd, max, ""))
  # min() and max() over the whole vector
  expect_equal(as.character(min(x[1:4], approx_range = 3)), "2001-02-26")
  expect_equal(as.character(max(x[c(1, 3)], approx_range = 3)), "2001-05-19")
  # as.Date() passes approx_range on to the rule
  expect_equal(as.Date(x[1], FUN = vmin, approx_range = 3), as.Date("2001-05-13"))
})

test_that("central tendency uses the trapezoidal weights of approximate dates", {
  y <- "2001-03-~01..2001-04-15"
  x <- as_messydate(c(y, "2001-05-16~", "2001-05-16"))
  m <- .weighted_members(y, 3)
  d <- as.numeric(as.Date(m$dates))
  w_mean <- as.character(as.Date(sum(d * m$w) / sum(m$w)))
  expect_equal(as.character(vmean(x, approx_range = 3)),
               c(w_mean, "2001-05-16", "2001-05-16"))
  cw <- cumsum(m$w) / sum(m$w)
  expect_equal(as.character(vmedian(x, approx_range = 3))[1], m$dates[which(cw >= 0.5)[1]])
  # without approx_range, the marks are ignored as before
  expect_equal(vmean(x, approx_range = 0), vmean(x))
  # the most likely dates are the core, so the mode does not change
  expect_equal(vmodal(x, approx_range = 3), vmodal(x))
  expect_equal(as.character(mean(x[2], approx_range = 3)), "2001-05-16")
  expect_equal(as.character(median(x[2], approx_range = 3)), "2001-05-16")
})

test_that("a set resolves to the earliest or latest day of any member", {
  x <- as_messydate(c("{2012-06,2013-01}", "{2013-01,2012-06}",
                      "[2013-01-05,2012-06-01]",
                      "{2012-06-01..2012-06-30,2012-01-15}",
                      "{-0050-01-01,-0049-06}"))
  expect_equal(as.character(vmin(x)),
               c("2012-06-01", "2012-06-01", "2012-06-01", "2012-01-15",
                 "-0050-01-01"))
  expect_equal(as.character(vmax(x)),
               c("2013-01-31", "2013-01-31", "2013-01-05", "2012-06-30",
                 "-0049-06-30"))
  expect_equal(as.character(max(x)), "2013-01-31")
  expect_equal(as.character(min(x[1:4])), "2012-01-15")
})

test_that("range() gives min() and max() together", {
  x <- as_messydate(c("2012", "2011-06", "2013-01-05"))
  expect_equal(range(x), vctrs::vec_c(min(x), max(x)))
  expect_equal(as.character(range(x)), c("2011-06-01", "2013-01-05"))
  expect_equal(as.character(range(as_messydate(c("2012", NA)), na.rm = TRUE)),
               c("2012-01-01", "2012-12-31"))
})

test_that("a trailing XX resolves as the shorter date", {
  x <- as_messydate(c("1993-01-XX", "1993-XX-XX", "1993-01-XX..1993-03-XX",
                      "{1993-01-XX,1993-02-05}", "1993-XX-15"))
  expect_equal(as.character(vmin(x)),
               c("1993-01-01", "1993-01-01", "1993-01-01", "1993-01-01", "1993-01-15"))
  expect_equal(as.character(vmax(x)),
               c("1993-01-31", "1993-12-31", "1993-03-31", "1993-02-05", "1993-12-15"))
  expect_true(as_messydate("1993-01-XX") < as_messydate("1993-06-15"))
  expect_equal(as.Date(as_messydate("1993-XX-XX"), FUN = vmin), as.Date("1993-01-01"))
})
