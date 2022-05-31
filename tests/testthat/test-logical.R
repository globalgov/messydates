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
