# Testing logical methods of messydates

test_that("is_messydate works", {
  expect_true(is_messydate(as_messydate("2012-01-01")))
  expect_false(is_messydate("2012-01-01"))
})

test_that("is_intersect works", {
  expect_true(is_intersecting(as_messydate("2012-01"),  as_messydate("2012-01-01..2012-02-22")))
  expect_false(is_intersecting(as_messydate("2012-01"),
                               as_messydate("2012-02-01..2012-02-22")))
})

test_that("is_element works", {
  expect_true(is_element(as_messydate("2012-01-01"), as_messydate("2012-01")))
  expect_false(is_element(as_messydate("2012-01-01"), as_messydate("2012-02")))
})

test_that("is_similar works", {
  expect_true(is_similar(as_messydate("2012-06-02"), as_messydate("2012-02-06")))
  expect_false(is_similar(as_messydate("2012-06-22"), as_messydate("2012-02-06")))
})

test_that("is_precise works", {
  expect_true(is_precise(as_messydate("2012-06-02")))
  expect_false(is_precise(as_messydate("2012?-06-22")))
})

test_that("is_uncertain works", {
  expect_true(is_uncertain(as_messydate("2012-06-02?")))
  expect_false(is_uncertain(as_messydate("2012-06-22")))
})

test_that("is_approximate works", {
  expect_true(is_approximate(as_messydate("2012-06-02~")))
  expect_false(is_approximate(as_messydate("2012-06-22")))
})

test_that("Logical comparisons work", {
  expect_true(as_messydate("2012-06-02") < as_messydate("2012-06-03"))
  expect_false(as_messydate("2012-06-02") > as_messydate("2012-06-03"))
  expect_true(as_messydate("2012-06-02") >= as_messydate("2012-06-02"))
  expect_true(as_messydate("2012-06-02") <= as_messydate("2012-06-02"))
  expect_equal(as_messydate("2012-06-02") >= as_messydate("2012-06-XX"), NA)
  expect_true(as_messydate("2012-06-30") >= as_messydate("2012-06-XX"))
  expect_equal(as_messydate("2012-06-02") < as_messydate("2012-06-XX"), NA)
  expect_true(as.Date("2012-06-01") <= as_messydate("2012-06-XX"))
  expect_true(as.POSIXct("2012-06-01") <= as_messydate("2012-06-XX"))
  expect_equal(as.Date("2012-06-01") >= as_messydate("2012-06-XX"), NA)
  expect_equal(as.POSIXct("2012-06-01") >= as_messydate("2012-06-XX"), NA)
  expect_equal(as_messydate("2012-06-XX") >= as.Date("2012-06-02"), NA)
  expect_equal(as_messydate("2012-06-XX") >= as.POSIXct("2012-06-02"), NA)
})

test_that("Logical comparisons don't mess up comparisons between non-messy times", {
  d1 <- as.Date(c("2012-01-01", "2012-02-01", "2012-03-01"))
  d2 <- as.Date(c("2012-03-01", "2012-02-01", "2012-01-01"))
  p1 <- as.POSIXct(c("2012-01-01", "2012-02-01", "2012-03-01"))
  p2 <- as.POSIXct(c("2012-03-01", "2012-02-01", "2012-01-01"))
  expect_identical(d1 < d2, c(TRUE, FALSE, FALSE))
  expect_identical(d1 > d2, c(FALSE, FALSE, TRUE))
  expect_identical(d1 <= d2, c(TRUE, TRUE, FALSE))
  expect_identical(d1 >= d2, c(FALSE, TRUE, TRUE))
  expect_identical(p1 < p2, c(TRUE, FALSE, FALSE))
  expect_identical(p1 > p2, c(FALSE, FALSE, TRUE))
  expect_identical(p1 <= p2, c(TRUE, TRUE, FALSE))
  expect_identical(p1 >= p2, c(FALSE, TRUE, TRUE))
  expect_identical(d1 < p2, c(TRUE, FALSE, FALSE))
  expect_identical(d1 > p2, c(FALSE, FALSE, TRUE))
  expect_identical(d1 <= p2, c(TRUE, TRUE, FALSE))
  expect_identical(d1 >= p2, c(FALSE, TRUE, TRUE))
  expect_identical(p1 < d2, c(TRUE, FALSE, FALSE))
  expect_identical(p1 > d2, c(FALSE, FALSE, TRUE))
  expect_identical(p1 <= d2, c(TRUE, TRUE, FALSE))
  expect_identical(p1 >= d2, c(FALSE, TRUE, TRUE))
})

test_that("Operand for is_element and is_intersect works", {
  expect_true(as_messydate("2002-02-02") %e% as_messydate("2002"))
  expect_equal(as_messydate("2002-02-02") %e% as_messydate("2002"),
               is_element(as_messydate("2002-02-02"), as_messydate("2002")))
  expect_true(as_messydate("2002-02-02") %i% as_messydate("2002"))
  expect_equal(as_messydate("2002-02-02") %i% as_messydate("2002"),
               is_element(as_messydate("2002-02-02"), as_messydate("2002")))
})
