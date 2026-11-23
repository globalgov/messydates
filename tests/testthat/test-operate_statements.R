# Testing logical methods of messydates
test_that("is_messydate works", {
  expect_true(is_messydate(as_messydate("2012-01-01")))
  expect_false(is_messydate("2012-01-01"))
})

test_that("date_is_intersecting works element-wise", {
  expect_true(date_is_intersecting(as_messydate("2012-01"),  as_messydate("2012-01-01..2012-02-22")))
  expect_false(date_is_intersecting(as_messydate("2012-01"),
                                    as_messydate("2012-02-01..2012-02-22")))
  expect_equal(date_is_intersecting(c("2012-01", "2012-01", "{}", NA),
                                    c("2012-01-15", "2012-03", "2012", "2012")),
               c(TRUE, FALSE, FALSE, NA))
  # y is recycled
  expect_equal(date_is_intersecting(c("2012-01-05", "2013"), "2012-01"),
               c(TRUE, FALSE))
})

test_that("date_is_subset works element-wise", {
  expect_true(date_is_subset(as_messydate("2012-01-01"), as_messydate("2012-01")))
  expect_true(date_is_subset(as_messydate("2012-01-01..2012-01-03"), as_messydate("2012-01")))
  expect_false(date_is_subset(as_messydate("2012-01-01"), as_messydate("2012-02")))
  # an overlap is not enough: all dates of x must be in y
  expect_false(date_is_subset("2012-01-01..2012-01-10", "2012-01-05"))
  expect_equal(date_is_subset(c("2012-01-05", "2013", "{}", NA),
                              c("2012-01", "2013-06", "2012", "2012")),
               c(TRUE, FALSE, TRUE, NA))
})

test_that("the old set predicate names still work with one message", {
  .deprecated_env <- getFromNamespace(".deprecated_env", "messydates")
  rm(list = intersect(c("is_intersecting()", "is_subset()"), ls(.deprecated_env)),
     envir = .deprecated_env)
  expect_message(x <- is_subset("2012-01-01", "2012-01"), "date_is_subset")
  expect_true(x)
  expect_no_message(is_subset("2012-01-01", "2012-01"))
  expect_message(x <- is_intersecting("2012-01", "2012-01-05"), "date_is_intersecting")
  expect_true(x)
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
  expect_identical(as.character(tag_approximate(x)), c("{}", "2012-01-~05"))
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

test_that("date_is_open finds open ranges", {
  expect_equal(date_is_open(as_messydate(c("..2012", "2012..", "2010..2012",
                                           "2012", "{..2010,2012}", NA))),
               c(TRUE, TRUE, FALSE, FALSE, TRUE, NA))
})
