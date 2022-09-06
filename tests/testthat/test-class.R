test_that("new_messydate works", {
  expect_equal(unclass(as_messydate("2012-01-01")), "2012-01-01")
  expect_equal(unclass(as_messydate("2012-1-1")), "2012-01-01")
})

test_that("incompleteness works", {
  expect_equal(unclass(as_messydate("NA-01-01")), "XXXX-01-01")
  expect_equal(unclass(as_messydate("NA-NA-01")), "XXXX-XX-01")
  expect_equal(unclass(as_messydate("2012-NA-01")), "2012-XX-01")
  expect_equal(unclass(as_messydate("2012-01-NA")), "2012-01")
  expect_equal(unclass(as_messydate("2012-1")), "2012-01")
  expect_equal(unclass(as_messydate("2012")), "2012")
  expect_equal(unclass(as_messydate("1")), "0001")
  expect_equal(unclass(as_messydate("12-10-93")), "93-10-12")
})

test_that("uncertainty works", {
  expect_equal(unclass(as_messydate("2012?-1-1")), "2012?-01-01")
  expect_equal(unclass(as_messydate("2012-01?-1")), "2012-01?-01")
  expect_equal(unclass(as_messydate("2012-1-01?")), "2012-01-01?")
})

test_that("approximation works", {
  expect_equal(unclass(as_messydate("2012~-1-1")), "2012~-01-01")
  expect_equal(unclass(as_messydate("2012-01~-01")), "2012-01~-01")
  expect_equal(unclass(as_messydate("2012-1-01~")), "2012-01-01~")
})

test_that("ranges work", {
  expect_equal(unclass(as_messydate("2012-01-01:2014-01-01")),
               "2012-01-01..2014-01-01")
  expect_equal(unclass(as_messydate("2012-01-01..2014-01-01")),
               "2012-01-01..2014-01-01")
  expect_equal(unclass(as_messydate("2012-01-01_2014-01-01")),
               "2012-01-01..2014-01-01")
})

test_that("negative works", {
  expect_equal(unclass(as_messydate("28 BC")), "-0028")
  expect_equal(unclass(as_messydate("200 BC:100 BC")), "-0200..-0100")
  expect_equal(unclass(as_messydate("{-200, -100}")), "{-0200,-0100}")
})

test_that("validate_messydate works", {
  expect_equal(validate_messydate(as_messydate("28 BC")), as_messydate("28 BC"))
  expect_error(validate_messydate(as_messydate("28 BCK")))
  expect_error(validate_messydate(as_messydate("X")))
  expect_error(validate_messydate(as_messydate("28>>")))
})

test_that("print method works", {
  expect_output(print(as_messydate("28 BC")), "-0028")
})
