# Testing logical methods of messydates
test_that("is_messydate works", {
  expect_true(is_messydate(as_messydate("2012-01-01")))
  expect_false(is_messydate("2012-01-01"))
})

test_that("is_intersect works", {
  expect_true(is_intersecting(as_messydate("2012-01"),  as_messydate("2012-01-01..2012-02-22")))
  # expect_false(is_intersecting(as_messydate("2012-01"),
  #                              as_messydate("2012-02-01..2012-02-22")))
})

test_that("is_subset works", {
  expect_true(is_subset(as_messydate("2012-01-01"), as_messydate("2012-01")))
  expect_true(is_subset(as_messydate("2012-01-01..2012-01-03"), as_messydate("2012-01")))
  # expect_false(is_subset(as_messydate("2012-01-01"), as_messydate("2012-02")))
})

test_that("date_is_similar works", {
  expect_true(date_is_similar(as_messydate("2012-06-02"), as_messydate("2012-02-06")))
  # expect_false(date_is_similar(as_messydate("2012-06-22"), as_messydate("2012-02-06")))
})

test_that("date_is_precise works", {
  expect_true(date_is_precise(as_messydate("2012-06-02")))
  # expect_false(date_is_precise(as_messydate("2012?-06-22")))
})

test_that("date_is_uncertain works", {
  expect_true(date_is_uncertain(as_messydate("2012-06-02?")))
  # expect_false(date_is_uncertain(as_messydate("2012-06-22")))
})

test_that("date_is_approximate works", {
  expect_true(date_is_approximate(as_messydate("2012-06-02~")))
  # expect_false(date_is_approximate(as_messydate("2012-06-22")))
})


test_that("the empty set is read, kept, and detected", {
  x <- as_messydate(c("{}", "2012-01", NA))
  expect_identical(as.character(x), c("{}", "2012-01", NA))
  expect_identical(as.character(as_messydate(" { } ")), "{}")
  expect_silent(validate_messydate(x))
  expect_identical(date_is_empty(x), c(TRUE, FALSE, NA))
  # '[]' ("one member of" nothing) is not a valid value
  expect_error(validate_messydate(new_messydate("[]")))
})

test_that("functions accept the empty set", {
  x <- as_messydate(c("{}", "2012-01-05"))
  expect_identical(expand(x)[[1]], character(0))
  expect_identical(as.character(vmin(x)), c(NA, "2012-01-05"))
  expect_identical(as.character(vmax(x)), c(NA, "2012-01-05"))
  expect_identical(vmean(x), c(NA, "2012-01-05"))
  expect_identical(vmedian(x), c(NA, "2012-01-05"))
  expect_identical(vrandom(x), c(NA, "2012-01-05"))
  # reducers resolve the union, to which '{}' adds nothing
  expect_identical(as.character(min(x)), "2012-01-05")
  expect_identical(as.character(max(x)), "2012-01-05")
  expect_identical(mean(x), "2012-01-05")
  expect_true(is.na(min(as_messydate("{}"))))
  expect_identical(precision(x), c(NA, 1))
  expect_identical(x < as_messydate("2012-06-01"), c(NA, TRUE))
  expect_identical(as.character(x + 1), c("{}", "2012-01-06"))
  expect_identical(as.character(x - 1), c("{}", "2012-01-04"))
  expect_identical(as.character(approximate(x)), c("{}", "2012-01-05~"))
  expect_identical(as.character(contract(x)), c("{}", "2012-01-05"))
})

test_that("The old is_*() names give a message once and the same result", {
  x <- as_messydate(c("2012-06-02", "2012-06-02?", "~2012", "-2012-06-02"))
  .deprecated_env$`is_precise()` <- NULL
  expect_message(old <- is_precise(x), "date_is_precise")
  expect_identical(old, date_is_precise(x))
  expect_silent(is_precise(x))
  suppressMessages({
    expect_identical(is_uncertain(x), date_is_uncertain(x))
    expect_identical(is_approximate(x), date_is_approximate(x))
    expect_identical(is_bce(x), date_is_bce(x))
    expect_identical(is_similar(x, x), date_is_similar(x, x))
  })
})
