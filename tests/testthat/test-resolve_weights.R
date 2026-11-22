test_that("approximate values get trapezoidal weights", {
  m <- messydates:::.weighted_members("2012-06-~15", 1)
  expect_identical(m$dates, c("2012-06-14", "2012-06-15", "2012-06-16"))
  expect_equal(m$w, c(0.5, 1, 0.5))
  # the core is flat, and the support widens by whole units on each side
  y <- messydates:::.weighted_members("2012~", 1)
  expect_identical(range(y$dates), c("2011-01-01", "2013-12-31"))
  expect_true(all(y$w[substr(y$dates, 1, 4) == "2012"] == 1))
  expect_true(all(y$w[substr(y$dates, 1, 4) != "2012"] < 1))
  # a mark after the month covers the year and the month
  expect_identical(range(messydates:::.weighted_members("2012-06~", 1)$dates),
                   c("2011-05-01", "2013-07-31"))
  expect_identical(range(messydates:::.weighted_members("2012-~06-15", 1)$dates),
                   c("2012-05-15", "2012-07-15"))
  expect_identical(range(messydates:::.weighted_members("~2012-06-15", 1)$dates),
                   c("2011-06-15", "2013-06-15"))
  # values that are not approximate, or approx_range = 0, are flat
  expect_true(all(messydates:::.weighted_members("2012-06", 1)$w == 1))
  expect_length(messydates:::.weighted_members("2012-06-15~", 0)$dates, 1)
})

test_that("vrandom() draws from the trapezoid", {
  set.seed(42)
  d <- vrandom(as_messydate(rep("2012-06-~15", 300)), approx_range = 1)
  expect_setequal(unique(d), c("2012-06-14", "2012-06-15", "2012-06-16"))
  expect_gt(mean(d == "2012-06-15"), 0.4)
  # the default does not widen approximate values
  expect_identical(vrandom(as_messydate("2012-06-15~")), "2012-06-15")
})

test_that("uncertain_p gives NA for uncertain values in some draws", {
  x <- as_messydate(c("2012-06-15?", "2012-06-15%", "2012-06-15"))
  expect_identical(vrandom(x), rep("2012-06-15", 3))
  expect_identical(vrandom(x, uncertain_p = 0), c(NA, NA, "2012-06-15"))
  expect_identical(vrandom(x, uncertain_p = c(1, 0, 0)),
                   c("2012-06-15", NA, "2012-06-15"))
  set.seed(42)
  d <- vrandom(as_messydate(rep("2012-06-15?", 400)), uncertain_p = 0.7)
  expect_gt(mean(is.na(d)), 0.2)
  expect_lt(mean(is.na(d)), 0.4)
  expect_identical(random(x, uncertain_p = 0), "2012-06-15")
  expect_error(vrandom(x, uncertain_p = 2), "from 0 to 1")
  expect_error(vrandom(x, uncertain_p = c(0.5, 0.5)), "length")
})

test_that("uncertain = 'drop' leaves out uncertain values", {
  x <- as_messydate(c("2012-01-01?", "2012-06-15", "2013-01-01%"))
  expect_identical(as.character(vmin(x, uncertain = "drop")),
                   c(NA, "2012-06-15", NA))
  expect_identical(vmean(x, uncertain = "drop"), c(NA, "2012-06-15", NA))
  expect_identical(as.character(min(x, uncertain = "drop")), "2012-06-15")
  expect_identical(as.character(max(x, uncertain = "drop")), "2012-06-15")
  expect_identical(as.character(min(x)), "2012-01-01")
  expect_error(vmin(x, uncertain = "ignore"))
})

test_that("min() and max() give NA when no value is known", {
  x <- as_messydate(c(NA_character_, NA_character_))
  expect_true(is.na(min(x)))
  expect_true(is.na(max(x)))
})

test_that("min() and max() follow the base R rule for NA", {
  x <- as_messydate(c("2012", NA, "2010-05"))
  expect_true(is.na(min(x)))
  expect_true(is.na(max(x)))
  expect_equal(as.character(min(x, na.rm = TRUE)), "2010-05-01")
  expect_equal(as.character(max(x, na.rm = TRUE)), "2012-12-31")
})

test_that("approximate ranges get trapezoidal weights beyond each approximate end", {
  m <- .weighted_members("1990-03-~01..1990-04-~15", approx_range = 3)
  expect_equal(m$dates[1], "1990-02-26")
  expect_equal(m$dates[length(m$dates)], "1990-04-18")
  expect_equal(m$w[1:4], c(0.25, 0.5, 0.75, 1))
  expect_equal(utils::tail(m$w, 4), c(1, 0.75, 0.5, 0.25))
  expect_true(all(m$w[4:(length(m$w) - 3)] == 1))
  # an end without a mark does not widen
  m <- .weighted_members("1990-03-01..1990-04-~15", approx_range = 3)
  expect_equal(m$dates[1], "1990-03-01")
  set.seed(1)
  d <- vrandom(as_messydate(rep("2012-01-~10..2012-01-~12", 500)), approx_range = 2)
  expect_true(all(d >= "2012-01-08" & d <= "2012-01-14"))
  expect_true(any(d < "2012-01-10") && any(d > "2012-01-12"))
})
